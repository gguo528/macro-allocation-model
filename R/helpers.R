# ================================
# Helper Functions
# ================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readr)
  library(zoo)
  library(quantmod)
  library(xts)
  library(purrr)
})

Sys.setlocale("LC_TIME", "C")

# -----------------------------
# Load FRED fredgraph.csv
# Handles Excel serial dates or date strings
# Forces month-end alignment
# -----------------------------
fred_load <- function(path_or_url, value_name = "value") {
  x <- read_csv(path_or_url, show_col_types = FALSE)
  
  names(x)[1:2] <- c("date_raw", value_name)
  
  x %>%
    mutate(
      date = case_when(
        is.numeric(date_raw) ~ as.Date(date_raw, origin = "1899-12-30"),
        TRUE ~ suppressWarnings(as.Date(date_raw))
      )
    ) %>%
    filter(!is.na(date)) %>%
    transmute(
      date = ceiling_date(date, "month") - days(1),
      !!value_name := as.numeric(.data[[value_name]])
    ) %>%
    distinct(date, .keep_all = TRUE) %>%
    arrange(date)
}

# -----------------------------
# Annualized multi-month growth
# Example: 3-month annualized inflation / growth
# -----------------------------
ann_growth <- function(x, k) {
  100 * ((x / dplyr::lag(x, k))^(12 / k) - 1)
}

# -----------------------------
# Safer Yahoo downloader
# Returns named xts objects in env
# -----------------------------
safe_getsymbols <- function(symbols,
                            from = "2000-01-01",
                            env = .GlobalEnv,
                            max_attempts = 3) {
  for (sym in symbols) {
    success <- FALSE
    
    for (i in seq_len(max_attempts)) {
      out <- try(
        getSymbols(
          Symbols = sym,
          src = "yahoo",
          from = from,
          auto.assign = FALSE,
          warnings = FALSE
        ),
        silent = TRUE
      )
      
      if (!inherits(out, "try-error")) {
        assign(sym, out, envir = env)
        success <- TRUE
        break
      }
    }
    
    if (!success) {
      warning(paste("Failed to download:", sym))
    }
  }
}

# -----------------------------
# Convert adjusted prices to month-end returns
# -----------------------------
monthly_returns_from_xts <- function(x, col_name) {
  px <- Ad(x)
  monthly_px <- to.monthly(px, indexAt = "lastof", OHLC = FALSE)
  
  tibble(
    date = as.Date(index(monthly_px)),
    !!col_name := as.numeric(monthly_px[, 1] / dplyr::lag(monthly_px[, 1], 1) - 1)
  ) %>%
    arrange(date)
}

# -----------------------------
# Rolling z-score using trailing window
# Uses only information available up to time t
# -----------------------------
roll_zscore <- function(x, window = 36) {
  rollapplyr(
    data = x,
    width = window,
    FUN = function(y) {
      s <- sd(y, na.rm = TRUE)
      m <- mean(y, na.rm = TRUE)
      last_y <- tail(y, 1)
      
      if (is.na(s) || s == 0) {
        return(NA_real_)
      } else {
        return((last_y - m) / s)
      }
    },
    fill = NA,
    partial = FALSE
  )
}

# -----------------------------
# 12-1 momentum
# Cumulative return from t-12 to t-1
# Excludes most recent month
# -----------------------------
calc_12_1_mom <- function(ret) {
  dplyr::lag(
    rollapplyr(
      1 + ret,
      width = 11,
      FUN = function(x) prod(x, na.rm = FALSE),
      fill = NA,
      partial = FALSE
    ) - 1,
    1
  )
}

# -----------------------------
# Clamp values into a range
# -----------------------------
clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

# -----------------------------
# Performance statistics
# -----------------------------
perf_stats <- function(ret_vec) {
  ret_vec <- ret_vec[is.finite(ret_vec)]
  
  n_months <- length(ret_vec)
  ann_return <- (prod(1 + ret_vec)^(12 / n_months)) - 1
  ann_vol <- sd(ret_vec) * sqrt(12)
  sharpe <- ifelse(ann_vol == 0, NA_real_, ann_return / ann_vol)
  
  wealth <- cumprod(1 + ret_vec)
  drawdown <- wealth / cummax(wealth) - 1
  max_dd <- min(drawdown, na.rm = TRUE)
  
  downside_sd <- sd(if_else(ret_vec < 0, ret_vec, 0), na.rm = TRUE) * sqrt(12)
  sortino <- ifelse(downside_sd == 0, NA_real_, ann_return / downside_sd)
  
  calmar <- ifelse(max_dd == 0, NA_real_, ann_return / abs(max_dd))
  hit_rate <- mean(ret_vec > 0, na.rm = TRUE)
  best_month <- max(ret_vec, na.rm = TRUE)
  worst_month <- min(ret_vec, na.rm = TRUE)
  
  tibble(
    Months = n_months,
    CAGR = ann_return,
    AnnVol = ann_vol,
    Sharpe = sharpe,
    Sortino = sortino,
    Calmar = calmar,
    MaxDrawdown = max_dd,
    HitRate = hit_rate,
    BestMonth = best_month,
    WorstMonth = worst_month
  )
}