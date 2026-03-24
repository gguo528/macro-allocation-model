# ================================
# V1 Macro Allocation Model
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

source("R/helpers.R")

# =========================================================
# Step 1: Pull and prepare macro data
# =========================================================

fred_urls <- list(
  CPIAUCSL   = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL",
  CPILFESL   = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPILFESL",
  UNRATE     = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=UNRATE",
  FEDFUNDS   = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=FEDFUNDS",
  GS10       = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS10",
  GS2        = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GS2",
  TB3MS      = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=TB3MS",
  PAYEMS     = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=PAYEMS",
  INDPRO     = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=INDPRO",
  HOUST      = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=HOUST",
  UMCSENT    = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=UMCSENT",
  BAA        = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=BAA",
  AAA        = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=AAA"
)

cpi_headline <- fred_load(fred_urls$CPIAUCSL, "cpi")
cpi_core     <- fred_load(fred_urls$CPILFESL, "core_cpi")
unrate       <- fred_load(fred_urls$UNRATE,   "unrate")
fedfunds     <- fred_load(fred_urls$FEDFUNDS, "fedfunds")
gs10         <- fred_load(fred_urls$GS10,     "gs10")
gs2          <- fred_load(fred_urls$GS2,      "gs2")
tb3ms        <- fred_load(fred_urls$TB3MS,    "tb3ms")
payems       <- fred_load(fred_urls$PAYEMS,   "payems")
indpro       <- fred_load(fred_urls$INDPRO,   "indpro")
houst        <- fred_load(fred_urls$HOUST,    "houst")
umcsent      <- fred_load(fred_urls$UMCSENT,  "umcsent")
baa          <- fred_load(fred_urls$BAA,      "baa")
aaa          <- fred_load(fred_urls$AAA,      "aaa")

macro_raw <- cpi_headline %>%
  full_join(cpi_core, by = "date") %>%
  full_join(unrate,   by = "date") %>%
  full_join(fedfunds, by = "date") %>%
  full_join(gs10,     by = "date") %>%
  full_join(gs2,      by = "date") %>%
  full_join(tb3ms,    by = "date") %>%
  full_join(payems,   by = "date") %>%
  full_join(indpro,   by = "date") %>%
  full_join(houst,    by = "date") %>%
  full_join(umcsent,  by = "date") %>%
  full_join(baa,      by = "date") %>%
  full_join(aaa,      by = "date") %>%
  arrange(date)

macro_monthly <- macro_raw %>%
  filter(date >= as.Date("1995-01-31")) %>%
  mutate(
    cpi_yoy               = 100 * (cpi / lag(cpi, 12) - 1),
    core_cpi_yoy          = 100 * (core_cpi / lag(core_cpi, 12) - 1),
    cpi_3m_ann            = ann_growth(cpi, 3),
    core_cpi_3m_ann       = ann_growth(core_cpi, 3),
    cpi_yoy_6m_delta      = cpi_yoy - lag(cpi_yoy, 6),
    core_cpi_yoy_6m_delta = core_cpi_yoy - lag(core_cpi_yoy, 6),
    unrate_3m_avg         = rollmean(unrate, 3, fill = NA, align = "right"),
    unrate_6m_delta       = unrate - lag(unrate, 6),
    payems_yoy            = 100 * (payems / lag(payems, 12) - 1),
    payems_3m_ann         = ann_growth(payems, 3),
    indpro_yoy            = 100 * (indpro / lag(indpro, 12) - 1),
    indpro_3m_ann         = ann_growth(indpro, 3),
    houst_yoy             = 100 * (houst / lag(houst, 12) - 1),
    umcsent_6m_delta      = umcsent - lag(umcsent, 6),
    real_fedfunds         = fedfunds - cpi_yoy,
    real_10y              = gs10 - cpi_yoy,
    term_spread_10y3m     = gs10 - tb3ms,
    term_spread_10y2y     = gs10 - gs2,
    front_end_spread_2y3m = gs2 - tb3ms,
    baa_aaa_spread        = baa - aaa,
    baa_10y_spread        = baa - gs10
  ) %>%
  mutate(
    cpi_yoy_6m_delta_smooth      = rollmean(cpi_yoy_6m_delta, 3, fill = NA, align = "right"),
    core_cpi_yoy_6m_delta_smooth = rollmean(core_cpi_yoy_6m_delta, 3, fill = NA, align = "right"),
    unrate_6m_delta_smooth       = rollmean(unrate_6m_delta, 3, fill = NA, align = "right"),
    payems_3m_ann_smooth         = rollmean(payems_3m_ann, 3, fill = NA, align = "right"),
    indpro_3m_ann_smooth         = rollmean(indpro_3m_ann, 3, fill = NA, align = "right"),
    houst_yoy_smooth             = rollmean(houst_yoy, 3, fill = NA, align = "right"),
    baa_10y_spread_smooth        = rollmean(baa_10y_spread, 3, fill = NA, align = "right"),
    inflation_trend_up = if_else(cpi_yoy_6m_delta_smooth > 0, 1, 0, missing = NA_integer_),
    labor_trend_up     = if_else(unrate_6m_delta_smooth < 0, 1, 0, missing = NA_integer_),
    growth_trend_up    = if_else(
      payems_3m_ann_smooth > 0 & indpro_3m_ann_smooth > 0,
      1, 0, missing = NA_integer_
    ),
    curve_positive     = if_else(term_spread_10y3m > 0, 1, 0, missing = NA_integer_),
    credit_loose       = if_else(baa_10y_spread_smooth < lag(baa_10y_spread_smooth, 6), 1, 0, missing = NA_integer_)
  ) %>%
  select(
    date, cpi, core_cpi, unrate, fedfunds, gs10, gs2, tb3ms,
    payems, indpro, houst, umcsent, baa, aaa,
    cpi_yoy, core_cpi_yoy, cpi_3m_ann, core_cpi_3m_ann,
    cpi_yoy_6m_delta, core_cpi_yoy_6m_delta,
    cpi_yoy_6m_delta_smooth, core_cpi_yoy_6m_delta_smooth,
    unrate_3m_avg, unrate_6m_delta, unrate_6m_delta_smooth,
    payems_yoy, payems_3m_ann, payems_3m_ann_smooth,
    indpro_yoy, indpro_3m_ann, indpro_3m_ann_smooth,
    houst_yoy, houst_yoy_smooth, umcsent_6m_delta,
    real_fedfunds, real_10y,
    term_spread_10y3m, term_spread_10y2y, front_end_spread_2y3m,
    baa_aaa_spread, baa_10y_spread, baa_10y_spread_smooth,
    inflation_trend_up, labor_trend_up, growth_trend_up,
    curve_positive, credit_loose
  ) %>%
  arrange(date)

write_csv(macro_monthly, "outputs/macro_monthly_step1_improved.csv")

# =========================================================
# Step 2: Convert macro features into regime signals
# =========================================================

macro_regimes <- macro_monthly %>%
  mutate(
    # --------------------------------------------------
    # 1. Inflation score
    # Higher score = inflation running hotter / worsening
    # --------------------------------------------------
    inflation_score =
      1 * if_else(cpi_yoy > 3, 1, 0, missing = NA_real_) +
      1 * if_else(core_cpi_yoy > 3, 1, 0, missing = NA_real_) +
      1 * if_else(cpi_3m_ann > cpi_yoy, 1, 0, missing = NA_real_) +
      1 * if_else(cpi_yoy_6m_delta_smooth > 0, 1, 0, missing = NA_real_),
    
    # --------------------------------------------------
    # 2. Growth score
    # Higher score = stronger cyclical growth backdrop
    # --------------------------------------------------
    growth_score =
      1 * if_else(unrate_6m_delta_smooth < 0, 1, 0, missing = NA_real_) +
      1 * if_else(payems_3m_ann_smooth > 0, 1, 0, missing = NA_real_) +
      1 * if_else(indpro_3m_ann_smooth > 0, 1, 0, missing = NA_real_) +
      1 * if_else(houst_yoy_smooth > 0, 1, 0, missing = NA_real_) +
      1 * if_else(umcsent_6m_delta > 0, 1, 0, missing = NA_real_),
    
    # --------------------------------------------------
    # 3. Policy score
    # Higher score = more restrictive monetary backdrop
    # --------------------------------------------------
    policy_score =
      1 * if_else(real_fedfunds > 0, 1, 0, missing = NA_real_) +
      1 * if_else(fedfunds > lag(fedfunds, 6), 1, 0, missing = NA_real_) +
      1 * if_else(real_10y > 1, 1, 0, missing = NA_real_),
    
    # --------------------------------------------------
    # 4. Curve score
    # Higher score = healthier term structure
    # --------------------------------------------------
    curve_score =
      1 * if_else(term_spread_10y3m > 0, 1, 0, missing = NA_real_) +
      1 * if_else(term_spread_10y2y > 0, 1, 0, missing = NA_real_) +
      1 * if_else(front_end_spread_2y3m > 0, 1, 0, missing = NA_real_),
    
    # --------------------------------------------------
    # 5. Credit score
    # Higher score = easier / less stressed credit backdrop
    # --------------------------------------------------
    credit_score =
      1 * if_else(baa_aaa_spread < lag(baa_aaa_spread, 6), 1, 0, missing = NA_real_) +
      1 * if_else(baa_10y_spread_smooth < lag(baa_10y_spread_smooth, 6), 1, 0, missing = NA_real_) +
      1 * if_else(baa_10y_spread < 3, 1, 0, missing = NA_real_),
    
    # --------------------------------------------------
    # 6. Binary state buckets from the scores
    # --------------------------------------------------
    inflation_state = case_when(
      inflation_score >= 3 ~ "High",
      inflation_score <= 1 ~ "Low",
      TRUE ~ "Moderate"
    ),
    
    growth_state = case_when(
      growth_score >= 4 ~ "Strong",
      growth_score <= 1 ~ "Weak",
      TRUE ~ "Moderate"
    ),
    
    policy_state = case_when(
      policy_score >= 2 ~ "Restrictive",
      policy_score == 1 ~ "Neutral",
      TRUE ~ "Easy"
    ),
    
    curve_state = case_when(
      curve_score >= 2 ~ "Healthy",
      curve_score == 1 ~ "Flat",
      TRUE ~ "Inverted"
    ),
    
    credit_state = case_when(
      credit_score >= 2 ~ "Easy",
      credit_score == 1 ~ "Mixed",
      TRUE ~ "Tight"
    ),
    
    # --------------------------------------------------
    # 7. Primary inflation-growth regime
    # This is the main economic quadrant
    # --------------------------------------------------
    primary_regime = case_when(
      growth_state == "Strong"   & inflation_state == "Low"      ~ "Goldilocks",
      growth_state == "Strong"   & inflation_state %in% c("Moderate", "High") ~ "Reflation",
      growth_state == "Weak"     & inflation_state == "High"     ~ "Stagflation",
      growth_state == "Weak"     & inflation_state %in% c("Low", "Moderate") ~ "Slowdown",
      growth_state == "Moderate" & inflation_state == "High"     ~ "LateCycle",
      growth_state == "Moderate" & inflation_state == "Low"      ~ "Recovery",
      TRUE ~ "Transition"
    ),
    
    # --------------------------------------------------
    # 8. Risk overlay regime
    # Distinguishes benign vs stressed versions
    # of the same inflation/growth quadrant
    # --------------------------------------------------
    risk_overlay = case_when(
      policy_state == "Restrictive" & curve_state == "Inverted" & credit_state == "Tight" ~ "Stress",
      policy_state == "Restrictive" & curve_state == "Inverted"                             ~ "Caution",
      credit_state == "Tight"                                                           ~ "Caution",
      curve_state == "Healthy" & credit_state == "Easy"                                 ~ "Supportive",
      TRUE                                                                                  ~ "Neutral"
    ),
    
    # --------------------------------------------------
    # 9. Final regime label
    # --------------------------------------------------
    regime = case_when(
      primary_regime == "Goldilocks" & risk_overlay %in% c("Supportive", "Neutral") ~ "Goldilocks",
      primary_regime == "Reflation"  & risk_overlay %in% c("Supportive", "Neutral") ~ "Reflation",
      primary_regime == "Reflation"  & risk_overlay %in% c("Caution", "Stress")     ~ "Overheat",
      primary_regime == "Slowdown"   & risk_overlay == "Supportive"                  ~ "SoftLanding",
      primary_regime == "Slowdown"   & risk_overlay %in% c("Neutral", "Caution")     ~ "Slowdown",
      primary_regime == "Slowdown"   & risk_overlay == "Stress"                      ~ "HardLanding",
      primary_regime == "Stagflation"                                                 ~ "Stagflation",
      primary_regime == "LateCycle"   & risk_overlay %in% c("Caution", "Stress")     ~ "LateCycle",
      primary_regime == "Recovery"                                                    ~ "Recovery",
      TRUE                                                                            ~ "Transition"
    ),
    
    # --------------------------------------------------
    # 10. Composite macro score
    # Higher = more risk-friendly backdrop
    # Lower = more defensive backdrop
    # --------------------------------------------------
    macro_score =
      1.25 * growth_score +
      0.75 * curve_score +
      0.75 * credit_score -
      1.00 * inflation_score -
      1.00 * policy_score,
    
    macro_bucket = case_when(
      macro_score >= 3  ~ "RiskOn",
      macro_score <= -1 ~ "RiskOff",
      TRUE              ~ "Neutral"
    ),
    
    # --------------------------------------------------
    # 11. Simple directional signals for continuity
    # --------------------------------------------------
    inflation_up = if_else(inflation_state == "High", 1, 0, missing = NA_integer_),
    growth_up    = if_else(growth_state == "Strong", 1, 0, missing = NA_integer_)
  ) %>%
  arrange(date)

write_csv(macro_regimes, "macro_regimes_step2_improved.csv")

# =========================================================
# Step 3: Pull asset prices and build monthly return table
# =========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(quantmod)
  library(xts)
  library(purrr)
})

Sys.setlocale("LC_TIME", "C")

# -----------------------------
# Asset universe
# -----------------------------
tickers <- c(
  "SPY", "QQQ", "IWM",
  "EFA", "EEM",
  "IEF", "TLT",
  "LQD", "HYG",
  "TIP", "GLD", "DBC", "VNQ",
  "BIL"
)

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

safe_getsymbols(tickers, from = "2000-01-01")

# -----------------------------
# Helper:
# Convert adjusted prices to month-end returns
# -----------------------------
monthly_returns_from_xts <- function(x, col_name) {
  px <- Ad(x)
  monthly_px <- to.monthly(px, indexAt = "lastof", OHLC = FALSE)
  
  tibble(
    date = as.Date(index(monthly_px)),
    !!col_name := as.numeric(monthly_px[, 1] / lag(monthly_px[, 1], 1) - 1)
  ) %>%
    arrange(date)
}

# -----------------------------
# Build monthly return tables
# Only create table if object exists
# -----------------------------
asset_return_list <- list(
  if (exists("SPY")) monthly_returns_from_xts(SPY, "spy_ret"),
  if (exists("QQQ")) monthly_returns_from_xts(QQQ, "qqq_ret"),
  if (exists("IWM")) monthly_returns_from_xts(IWM, "iwm_ret"),
  if (exists("EFA")) monthly_returns_from_xts(EFA, "efa_ret"),
  if (exists("EEM")) monthly_returns_from_xts(EEM, "eem_ret"),
  if (exists("IEF")) monthly_returns_from_xts(IEF, "ief_ret"),
  if (exists("TLT")) monthly_returns_from_xts(TLT, "tlt_ret"),
  if (exists("LQD")) monthly_returns_from_xts(LQD, "lqd_ret"),
  if (exists("HYG")) monthly_returns_from_xts(HYG, "hyg_ret"),
  if (exists("TIP")) monthly_returns_from_xts(TIP, "tip_ret"),
  if (exists("GLD")) monthly_returns_from_xts(GLD, "gld_ret"),
  if (exists("DBC")) monthly_returns_from_xts(DBC, "dbc_ret"),
  if (exists("VNQ")) monthly_returns_from_xts(VNQ, "vnq_ret"),
  if (exists("BIL")) monthly_returns_from_xts(BIL, "bil_ret")
) %>%
  compact()

# -----------------------------
# Merge all monthly return tables
# -----------------------------
asset_returns <- reduce(asset_return_list, full_join, by = "date") %>%
  arrange(date) %>%
  mutate(date = ceiling_date(date, "month") - days(1)) %>%
  filter(date >= as.Date("2004-01-31"))

# -----------------------------
# Optional: sleeve composites
# These help simplify later weight design
# -----------------------------
asset_returns <- asset_returns %>%
  mutate(
    us_equity_ret = rowMeans(select(., spy_ret, qqq_ret, iwm_ret), na.rm = TRUE),
    intl_equity_ret = rowMeans(select(., efa_ret, eem_ret), na.rm = TRUE),
    equity_ret = rowMeans(select(., spy_ret, qqq_ret, iwm_ret, efa_ret, eem_ret), na.rm = TRUE),
    
    treasury_ret = rowMeans(select(., ief_ret, tlt_ret), na.rm = TRUE),
    credit_ret   = rowMeans(select(., lqd_ret, hyg_ret), na.rm = TRUE),
    real_asset_ret = rowMeans(select(., tip_ret, gld_ret, dbc_ret, vnq_ret), na.rm = TRUE)
  )

# -----------------------------
# Optional: availability flags
# Useful later if you want to exclude
# periods before ETF inception from a sleeve
# -----------------------------
asset_returns <- asset_returns %>%
  mutate(
    n_equity_assets = rowSums(!is.na(select(., spy_ret, qqq_ret, iwm_ret, efa_ret, eem_ret))),
    n_treasury_assets = rowSums(!is.na(select(., ief_ret, tlt_ret))),
    n_credit_assets = rowSums(!is.na(select(., lqd_ret, hyg_ret))),
    n_real_assets = rowSums(!is.na(select(., tip_ret, gld_ret, dbc_ret, vnq_ret)))
  )

# -----------------------------
# Save intermediate file
# -----------------------------
write_csv(asset_returns, "asset_returns_step3_improved.csv")

# =========================================================
# Step 4: Join lagged macro regimes to asset returns
# =========================================================

# -----------------------------
# 4A. Align macro and asset data at month-end
# -----------------------------
macro_regimes_aligned <- macro_regimes %>%
  mutate(date = ceiling_date(date, "month") - days(1)) %>%
  arrange(date)

asset_returns_aligned <- asset_returns %>%
  mutate(date = ceiling_date(date, "month") - days(1)) %>%
  arrange(date)

# -----------------------------
# 4B. Create model table with lagged macro signals
# The portfolio formed for month t uses signals from month t-1
# -----------------------------
model_data <- macro_regimes_aligned %>%
  transmute(
    date,
    regime_signal       = lag(regime, 1),
    primary_signal      = lag(primary_regime, 1),
    overlay_signal      = lag(risk_overlay, 1),
    macro_bucket_signal = lag(macro_bucket, 1),
    macro_score_signal  = lag(macro_score, 1),
    inflation_signal    = lag(inflation_up, 1),
    growth_signal       = lag(growth_up, 1)
  ) %>%
  inner_join(asset_returns_aligned, by = "date") %>%
  arrange(date)

glimpse(model_data)
print(head(model_data, 20))
print(tail(model_data, 20))

write_csv(model_data, "model_data_step4_improved.csv")

# -----------------------------
# 4C. Regime-based target weights
# Weights are set at the ETF level
# They should sum to 1 within each regime
# -----------------------------
regime_weights <- tribble(
  ~regime_signal, ~w_spy, ~w_qqq, ~w_iwm, ~w_efa, ~w_eem, ~w_ief, ~w_tlt, ~w_lqd, ~w_hyg, ~w_tip, ~w_gld, ~w_dbc, ~w_vnq, ~w_bil,
  
  # Strong growth, tame inflation, supportive backdrop
  "Goldilocks",    0.22,   0.20,   0.08,   0.10,   0.05,   0.05,   0.00,   0.07,   0.10,   0.03,   0.03,   0.00,   0.05,   0.02,
  
  # Growth accelerating, inflation firming
  "Reflation",     0.16,   0.08,   0.14,   0.08,   0.10,   0.03,   0.00,   0.04,   0.10,   0.06,   0.08,   0.08,   0.04,   0.01,
  
  # Growth still decent but inflation/policy pressure is becoming a problem
  "Overheat",      0.10,   0.04,   0.08,   0.06,   0.06,   0.04,   0.00,   0.04,   0.06,   0.12,   0.14,   0.14,   0.05,   0.07,
  
  # Benign slowdown, not yet full stress
  "SoftLanding",   0.18,   0.10,   0.05,   0.08,   0.04,   0.12,   0.05,   0.08,   0.07,   0.06,   0.05,   0.02,   0.05,   0.05,
  
  # Clear slowdown, more defense
  "Slowdown",      0.10,   0.05,   0.03,   0.05,   0.02,   0.18,   0.14,   0.08,   0.03,   0.07,   0.07,   0.00,   0.03,   0.15,
  
  # Recession / stress / hard landing
  "HardLanding",   0.04,   0.02,   0.01,   0.02,   0.00,   0.22,   0.24,   0.05,   0.00,   0.05,   0.10,   0.00,   0.00,   0.25,
  
  # Weak growth + high inflation
  "Stagflation",   0.04,   0.01,   0.02,   0.02,   0.02,   0.05,   0.00,   0.02,   0.02,   0.16,   0.24,   0.18,   0.05,   0.17,
  
  # Mid-cycle recovery from weakness
  "Recovery",      0.20,   0.12,   0.10,   0.09,   0.06,   0.08,   0.02,   0.07,   0.09,   0.04,   0.04,   0.02,   0.05,   0.02,
  
  # Ambiguous state
  "Transition",    0.14,   0.08,   0.05,   0.07,   0.03,   0.12,   0.06,   0.08,   0.05,   0.06,   0.07,   0.02,   0.04,   0.13,
  
  # Late-cycle but not yet broken
  "LateCycle",     0.11,   0.05,   0.06,   0.06,   0.04,   0.08,   0.03,   0.06,   0.05,   0.10,   0.12,   0.10,   0.05,   0.09
)

# Quick integrity check
weight_check <- regime_weights %>%
  mutate(weight_sum = w_spy + w_qqq + w_iwm + w_efa + w_eem + w_ief + w_tlt +
           w_lqd + w_hyg + w_tip + w_gld + w_dbc + w_vnq + w_bil)

print(weight_check %>% select(regime_signal, weight_sum))

# -----------------------------
# 4D. Join weights to model data
# -----------------------------
backtest_data_raw <- model_data %>%
  left_join(regime_weights, by = "regime_signal") %>%
  arrange(date)

# -----------------------------
# 4E. Normalize weights over assets that actually exist that month
# This avoids broken early-history months where some ETFs had not launched yet
# -----------------------------
backtest_data <- backtest_data_raw %>%
  mutate(
    avail_spy = if_else(!is.na(spy_ret), 1, 0),
    avail_qqq = if_else(!is.na(qqq_ret), 1, 0),
    avail_iwm = if_else(!is.na(iwm_ret), 1, 0),
    avail_efa = if_else(!is.na(efa_ret), 1, 0),
    avail_eem = if_else(!is.na(eem_ret), 1, 0),
    avail_ief = if_else(!is.na(ief_ret), 1, 0),
    avail_tlt = if_else(!is.na(tlt_ret), 1, 0),
    avail_lqd = if_else(!is.na(lqd_ret), 1, 0),
    avail_hyg = if_else(!is.na(hyg_ret), 1, 0),
    avail_tip = if_else(!is.na(tip_ret), 1, 0),
    avail_gld = if_else(!is.na(gld_ret), 1, 0),
    avail_dbc = if_else(!is.na(dbc_ret), 1, 0),
    avail_vnq = if_else(!is.na(vnq_ret), 1, 0),
    avail_bil = if_else(!is.na(bil_ret), 1, 0),
    
    raw_weight_sum =
      w_spy * avail_spy +
      w_qqq * avail_qqq +
      w_iwm * avail_iwm +
      w_efa * avail_efa +
      w_eem * avail_eem +
      w_ief * avail_ief +
      w_tlt * avail_tlt +
      w_lqd * avail_lqd +
      w_hyg * avail_hyg +
      w_tip * avail_tip +
      w_gld * avail_gld +
      w_dbc * avail_dbc +
      w_vnq * avail_vnq +
      w_bil * avail_bil,
    
    w_spy_n = if_else(raw_weight_sum > 0, w_spy * avail_spy / raw_weight_sum, NA_real_),
    w_qqq_n = if_else(raw_weight_sum > 0, w_qqq * avail_qqq / raw_weight_sum, NA_real_),
    w_iwm_n = if_else(raw_weight_sum > 0, w_iwm * avail_iwm / raw_weight_sum, NA_real_),
    w_efa_n = if_else(raw_weight_sum > 0, w_efa * avail_efa / raw_weight_sum, NA_real_),
    w_eem_n = if_else(raw_weight_sum > 0, w_eem * avail_eem / raw_weight_sum, NA_real_),
    w_ief_n = if_else(raw_weight_sum > 0, w_ief * avail_ief / raw_weight_sum, NA_real_),
    w_tlt_n = if_else(raw_weight_sum > 0, w_tlt * avail_tlt / raw_weight_sum, NA_real_),
    w_lqd_n = if_else(raw_weight_sum > 0, w_lqd * avail_lqd / raw_weight_sum, NA_real_),
    w_hyg_n = if_else(raw_weight_sum > 0, w_hyg * avail_hyg / raw_weight_sum, NA_real_),
    w_tip_n = if_else(raw_weight_sum > 0, w_tip * avail_tip / raw_weight_sum, NA_real_),
    w_gld_n = if_else(raw_weight_sum > 0, w_gld * avail_gld / raw_weight_sum, NA_real_),
    w_dbc_n = if_else(raw_weight_sum > 0, w_dbc * avail_dbc / raw_weight_sum, NA_real_),
    w_vnq_n = if_else(raw_weight_sum > 0, w_vnq * avail_vnq / raw_weight_sum, NA_real_),
    w_bil_n = if_else(raw_weight_sum > 0, w_bil * avail_bil / raw_weight_sum, NA_real_),
    
    strategy_ret =
      coalesce(w_spy_n, 0) * coalesce(spy_ret, 0) +
      coalesce(w_qqq_n, 0) * coalesce(qqq_ret, 0) +
      coalesce(w_iwm_n, 0) * coalesce(iwm_ret, 0) +
      coalesce(w_efa_n, 0) * coalesce(efa_ret, 0) +
      coalesce(w_eem_n, 0) * coalesce(eem_ret, 0) +
      coalesce(w_ief_n, 0) * coalesce(ief_ret, 0) +
      coalesce(w_tlt_n, 0) * coalesce(tlt_ret, 0) +
      coalesce(w_lqd_n, 0) * coalesce(lqd_ret, 0) +
      coalesce(w_hyg_n, 0) * coalesce(hyg_ret, 0) +
      coalesce(w_tip_n, 0) * coalesce(tip_ret, 0) +
      coalesce(w_gld_n, 0) * coalesce(gld_ret, 0) +
      coalesce(w_dbc_n, 0) * coalesce(dbc_ret, 0) +
      coalesce(w_vnq_n, 0) * coalesce(vnq_ret, 0) +
      coalesce(w_bil_n, 0) * coalesce(bil_ret, 0),
    
    # Benchmarks
    benchmark_spy_ret   = spy_ret,
    benchmark_6040_ret  = 0.60 * coalesce(spy_ret, 0) + 0.40 * coalesce(ief_ret, 0),
    benchmark_allweather_ret =
      0.30 * coalesce(spy_ret, 0) +
      0.40 * coalesce(tlt_ret, 0) +
      0.15 * coalesce(tip_ret, 0) +
      0.075 * coalesce(gld_ret, 0) +
      0.075 * coalesce(dbc_ret, 0)
  ) %>%
  filter(
    !is.na(regime_signal),
    !is.na(strategy_ret),
    !is.na(benchmark_spy_ret)
  ) %>%
  arrange(date)

write_csv(backtest_data, "backtest_data_step4_improved.csv")

# =========================================================
# Step 7-20: Backtest, performance, diagnostics
# =========================================================

# =========================================================
# Step 7: Build cumulative performance series
# Includes gross and transaction-cost-adjusted paths
# =========================================================

# -----------------------------
# 7A. Turnover and transaction cost model
# Assumption:
# - turnover = 0.5 * sum(abs(w_t - w_{t-1}))
# - tc_cost = turnover * one_way_cost
# You can change one_way_cost to test sensitivity
# -----------------------------
one_way_cost <- 0.0010   # 10 bps one-way trading cost

backtest_data <- backtest_data %>%
  arrange(date) %>%
  mutate(
    turnover =
      0.5 * (
        abs(w_spy_n - lag(w_spy_n)) +
          abs(w_qqq_n - lag(w_qqq_n)) +
          abs(w_iwm_n - lag(w_iwm_n)) +
          abs(w_efa_n - lag(w_efa_n)) +
          abs(w_eem_n - lag(w_eem_n)) +
          abs(w_ief_n - lag(w_ief_n)) +
          abs(w_tlt_n - lag(w_tlt_n)) +
          abs(w_lqd_n - lag(w_lqd_n)) +
          abs(w_hyg_n - lag(w_hyg_n)) +
          abs(w_tip_n - lag(w_tip_n)) +
          abs(w_gld_n - lag(w_gld_n)) +
          abs(w_dbc_n - lag(w_dbc_n)) +
          abs(w_vnq_n - lag(w_vnq_n)) +
          abs(w_bil_n - lag(w_bil_n))
      ),
    
    turnover = replace_na(turnover, 0),
    tc_cost = turnover * one_way_cost,
    strategy_ret_net = strategy_ret - tc_cost
  )

# -----------------------------
# 7B. Cumulative index paths
# -----------------------------
performance_path <- backtest_data %>%
  arrange(date) %>%
  mutate(
    strategy_index_gross = 100 * cumprod(1 + strategy_ret),
    strategy_index_net   = 100 * cumprod(1 + strategy_ret_net),
    spy_index            = 100 * cumprod(1 + benchmark_spy_ret),
    bal6040_index        = 100 * cumprod(1 + benchmark_6040_ret),
    allweather_index     = 100 * cumprod(1 + benchmark_allweather_ret)
  )

glimpse(performance_path)
print(head(performance_path, 20))
print(tail(performance_path, 20))

write_csv(performance_path, "performance_path_step7_improved.csv")

# =========================================================
# Step 8: Plot cumulative performance
# =========================================================

plot_data <- performance_path %>%
  select(
    date,
    strategy_index_gross,
    strategy_index_net,
    spy_index,
    bal6040_index,
    allweather_index
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "index_value"
  )

ggplot(plot_data, aes(x = date, y = index_value, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Macro Regime Allocation Model vs Benchmarks",
    x = NULL,
    y = "Growth of 100",
    color = NULL
  ) +
  theme_minimal(base_size = 12)

# =========================================================
# Step 9: Performance metrics
# Expanded performance statistics
# =========================================================

perf_stats <- function(ret_vec) {
  ret_vec <- ret_vec[is.finite(ret_vec)]
  
  n_months <- length(ret_vec)
  
  ann_return <- (prod(1 + ret_vec)^(12 / n_months)) - 1
  ann_vol    <- sd(ret_vec) * sqrt(12)
  sharpe     <- ifelse(ann_vol == 0, NA_real_, ann_return / ann_vol)
  
  wealth      <- cumprod(1 + ret_vec)
  drawdown    <- wealth / cummax(wealth) - 1
  max_dd      <- min(drawdown, na.rm = TRUE)
  
  downside_sd <- sd(if_else(ret_vec < 0, ret_vec, 0), na.rm = TRUE) * sqrt(12)
  sortino     <- ifelse(downside_sd == 0, NA_real_, ann_return / downside_sd)
  
  calmar      <- ifelse(max_dd == 0, NA_real_, ann_return / abs(max_dd))
  hit_rate    <- mean(ret_vec > 0, na.rm = TRUE)
  best_month  <- max(ret_vec, na.rm = TRUE)
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

stats_strategy_gross <- perf_stats(performance_path$strategy_ret) %>%
  mutate(series = "Strategy_Gross")

stats_strategy_net <- perf_stats(performance_path$strategy_ret_net) %>%
  mutate(series = "Strategy_Net")

stats_spy <- perf_stats(performance_path$benchmark_spy_ret) %>%
  mutate(series = "SPY")

stats_6040 <- perf_stats(performance_path$benchmark_6040_ret) %>%
  mutate(series = "60_40")

stats_allweather <- perf_stats(performance_path$benchmark_allweather_ret) %>%
  mutate(series = "AllWeather")

stats_table <- bind_rows(
  stats_strategy_gross,
  stats_strategy_net,
  stats_spy,
  stats_6040,
  stats_allweather
) %>%
  select(series, everything())

write_csv(stats_table, "performance_stats_step9_improved.csv")

# =========================================================
# Step 10: Add drawdown paths
# =========================================================

performance_path <- performance_path %>%
  mutate(
    strategy_dd_gross = strategy_index_gross / cummax(strategy_index_gross) - 1,
    strategy_dd_net   = strategy_index_net / cummax(strategy_index_net) - 1,
    spy_dd            = spy_index / cummax(spy_index) - 1,
    bal6040_dd        = bal6040_index / cummax(bal6040_index) - 1,
    allweather_dd     = allweather_index / cummax(allweather_index) - 1
  )

write_csv(performance_path, "performance_path_step10_improved.csv")

# =========================================================
# Step 11: Plot drawdowns
# =========================================================

drawdown_plot_data <- performance_path %>%
  select(
    date,
    strategy_dd_gross,
    strategy_dd_net,
    spy_dd,
    bal6040_dd,
    allweather_dd
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "drawdown"
  )

ggplot(drawdown_plot_data, aes(x = date, y = drawdown, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Drawdowns: Strategy vs Benchmarks",
    x = NULL,
    y = "Drawdown",
    color = NULL
  ) +
  theme_minimal(base_size = 12)

# =========================================================
# Step 12: Regime diagnostics
# How often does each regime occur?
# What were average returns inside each regime?
# =========================================================

regime_summary <- backtest_data %>%
  group_by(regime_signal) %>%
  summarise(
    months = n(),
    
    avg_strategy_ret_gross = mean(strategy_ret, na.rm = TRUE),
    avg_strategy_ret_net   = mean(strategy_ret_net, na.rm = TRUE),
    
    avg_spy_ret = mean(spy_ret, na.rm = TRUE),
    avg_qqq_ret = mean(qqq_ret, na.rm = TRUE),
    avg_iwm_ret = mean(iwm_ret, na.rm = TRUE),
    avg_efa_ret = mean(efa_ret, na.rm = TRUE),
    avg_eem_ret = mean(eem_ret, na.rm = TRUE),
    avg_ief_ret = mean(ief_ret, na.rm = TRUE),
    avg_tlt_ret = mean(tlt_ret, na.rm = TRUE),
    avg_lqd_ret = mean(lqd_ret, na.rm = TRUE),
    avg_hyg_ret = mean(hyg_ret, na.rm = TRUE),
    avg_tip_ret = mean(tip_ret, na.rm = TRUE),
    avg_gld_ret = mean(gld_ret, na.rm = TRUE),
    avg_dbc_ret = mean(dbc_ret, na.rm = TRUE),
    avg_vnq_ret = mean(vnq_ret, na.rm = TRUE),
    avg_bil_ret = mean(bil_ret, na.rm = TRUE),
    
    avg_turnover = mean(turnover, na.rm = TRUE),
    strategy_minus_spy = mean(strategy_ret - benchmark_spy_ret, na.rm = TRUE),
    strategy_minus_6040 = mean(strategy_ret - benchmark_6040_ret, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(desc(months))

write_csv(regime_summary, "regime_summary_step12_improved.csv")

# =========================================================
# Step 13: Rolling 12-month returns and rolling relative returns
# =========================================================

performance_path <- performance_path %>%
  mutate(
    strategy_12m_gross = strategy_index_gross / lag(strategy_index_gross, 12) - 1,
    strategy_12m_net   = strategy_index_net / lag(strategy_index_net, 12) - 1,
    spy_12m            = spy_index / lag(spy_index, 12) - 1,
    bal6040_12m        = bal6040_index / lag(bal6040_index, 12) - 1,
    allweather_12m     = allweather_index / lag(allweather_index, 12) - 1,
    
    rel_12m_vs_spy     = strategy_12m_net - spy_12m,
    rel_12m_vs_6040    = strategy_12m_net - bal6040_12m,
    rel_12m_vs_aw      = strategy_12m_net - allweather_12m
  )

write_csv(performance_path, "performance_path_step13_improved.csv")

# =========================================================
# Step 14A: Plot rolling 12-month returns
# =========================================================

rolling_plot_data <- performance_path %>%
  select(
    date,
    strategy_12m_gross,
    strategy_12m_net,
    spy_12m,
    bal6040_12m,
    allweather_12m
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "rolling_return"
  ) %>%
  filter(!is.na(rolling_return))

ggplot(rolling_plot_data, aes(x = date, y = rolling_return, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Rolling 12-Month Returns",
    x = NULL,
    y = "Return",
    color = NULL
  ) +
  theme_minimal(base_size = 12)

# =========================================================
# Step 14B: Plot rolling relative performance
# =========================================================

rolling_relative_plot_data <- performance_path %>%
  select(date, rel_12m_vs_spy, rel_12m_vs_6040, rel_12m_vs_aw) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "relative_return"
  ) %>%
  filter(!is.na(relative_return))

ggplot(rolling_relative_plot_data, aes(x = date, y = relative_return, color = series)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  labs(
    title = "Rolling 12-Month Relative Performance",
    x = NULL,
    y = "Strategy minus Benchmark",
    color = NULL
  ) +
  theme_minimal(base_size = 12)

# =========================================================
# Step 15: Subperiod performance
# =========================================================

subperiod_stats <- performance_path %>%
  mutate(
    period = case_when(
      date < as.Date("2008-01-01") ~ "2004-2007",
      date < as.Date("2013-01-01") ~ "2008-2012",
      date < as.Date("2020-01-01") ~ "2013-2019",
      TRUE                         ~ "2020+"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    strategy_cagr_gross = (last(strategy_index_gross) / first(strategy_index_gross))^(12 / n()) - 1,
    strategy_cagr_net   = (last(strategy_index_net) / first(strategy_index_net))^(12 / n()) - 1,
    spy_cagr            = (last(spy_index) / first(spy_index))^(12 / n()) - 1,
    bal6040_cagr        = (last(bal6040_index) / first(bal6040_index))^(12 / n()) - 1,
    allweather_cagr     = (last(allweather_index) / first(allweather_index))^(12 / n()) - 1,
    avg_turnover        = mean(turnover, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(subperiod_stats, "subperiod_stats_step15.csv")

# =========================================================
# Step 16: Regime transition and turnover diagnostics
# =========================================================

regime_transition_stats <- backtest_data %>%
  mutate(
    prev_regime = lag(regime_signal),
    turnover_flag = if_else(regime_signal != prev_regime, 1, 0, missing = 0)
  ) %>%
  summarise(
    avg_monthly_turnover = mean(turnover, na.rm = TRUE),
    median_monthly_turnover = median(turnover, na.rm = TRUE),
    max_monthly_turnover = max(turnover, na.rm = TRUE),
    regime_switch_rate = mean(turnover_flag, na.rm = TRUE),
    avg_tc_cost = mean(tc_cost, na.rm = TRUE)
  )

write_csv(regime_transition_stats, "regime_transition_stats_step16.csv")

# =========================================================
# Step 17: Average exposures by regime
# =========================================================

regime_exposure_summary <- backtest_data %>%
  group_by(regime_signal) %>%
  summarise(
    w_spy = mean(w_spy_n, na.rm = TRUE),
    w_qqq = mean(w_qqq_n, na.rm = TRUE),
    w_iwm = mean(w_iwm_n, na.rm = TRUE),
    w_efa = mean(w_efa_n, na.rm = TRUE),
    w_eem = mean(w_eem_n, na.rm = TRUE),
    w_ief = mean(w_ief_n, na.rm = TRUE),
    w_tlt = mean(w_tlt_n, na.rm = TRUE),
    w_lqd = mean(w_lqd_n, na.rm = TRUE),
    w_hyg = mean(w_hyg_n, na.rm = TRUE),
    w_tip = mean(w_tip_n, na.rm = TRUE),
    w_gld = mean(w_gld_n, na.rm = TRUE),
    w_dbc = mean(w_dbc_n, na.rm = TRUE),
    w_vnq = mean(w_vnq_n, na.rm = TRUE),
    w_bil = mean(w_bil_n, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(regime_exposure_summary, "regime_exposure_summary_step17.csv")

# =========================================================
# Step 18: Plot allocation history
# =========================================================

allocation_plot_data <- backtest_data %>%
  select(
    date,
    w_spy_n, w_qqq_n, w_iwm_n, w_efa_n, w_eem_n,
    w_ief_n, w_tlt_n, w_lqd_n, w_hyg_n,
    w_tip_n, w_gld_n, w_dbc_n, w_vnq_n, w_bil_n
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "asset",
    values_to = "weight"
  )

ggplot(allocation_plot_data, aes(x = date, y = weight, color = asset)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Dynamic Portfolio Weights Through Time",
    x = NULL,
    y = "Portfolio Weight",
    color = NULL
  ) +
  theme_minimal(base_size = 12)

# =========================================================
# Step 19: Sleeve contribution analysis
# =========================================================

contribution_data <- backtest_data %>%
  mutate(
    contrib_us_equity =
      coalesce(w_spy_n, 0) * coalesce(spy_ret, 0) +
      coalesce(w_qqq_n, 0) * coalesce(qqq_ret, 0) +
      coalesce(w_iwm_n, 0) * coalesce(iwm_ret, 0),
    
    contrib_intl_equity =
      coalesce(w_efa_n, 0) * coalesce(efa_ret, 0) +
      coalesce(w_eem_n, 0) * coalesce(eem_ret, 0),
    
    contrib_treasury =
      coalesce(w_ief_n, 0) * coalesce(ief_ret, 0) +
      coalesce(w_tlt_n, 0) * coalesce(tlt_ret, 0),
    
    contrib_credit =
      coalesce(w_lqd_n, 0) * coalesce(lqd_ret, 0) +
      coalesce(w_hyg_n, 0) * coalesce(hyg_ret, 0),
    
    contrib_real_assets =
      coalesce(w_tip_n, 0) * coalesce(tip_ret, 0) +
      coalesce(w_gld_n, 0) * coalesce(gld_ret, 0) +
      coalesce(w_dbc_n, 0) * coalesce(dbc_ret, 0) +
      coalesce(w_vnq_n, 0) * coalesce(vnq_ret, 0),
    
    contrib_cash =
      coalesce(w_bil_n, 0) * coalesce(bil_ret, 0)
  )

contribution_summary <- contribution_data %>%
  summarise(
    us_equity = mean(contrib_us_equity, na.rm = TRUE),
    intl_equity = mean(contrib_intl_equity, na.rm = TRUE),
    treasury = mean(contrib_treasury, na.rm = TRUE),
    credit = mean(contrib_credit, na.rm = TRUE),
    real_assets = mean(contrib_real_assets, na.rm = TRUE),
    cash = mean(contrib_cash, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "sleeve",
    values_to = "avg_monthly_contribution"
  )

write_csv(contribution_summary, "contribution_summary_step19.csv")

# =========================================================
# Step 20: Regime hit-rate diagnostics
# Did the chosen regime beat SPY / 60-40 in that month?
# =========================================================

regime_hit_rates <- backtest_data %>%
  mutate(
    beat_spy = if_else(strategy_ret_net > benchmark_spy_ret, 1, 0, missing = NA_real_),
    beat_6040 = if_else(strategy_ret_net > benchmark_6040_ret, 1, 0, missing = NA_real_),
    beat_aw = if_else(strategy_ret_net > benchmark_allweather_ret, 1, 0, missing = NA_real_)
  ) %>%
  group_by(regime_signal) %>%
  summarise(
    months = n(),
    hit_rate_vs_spy = mean(beat_spy, na.rm = TRUE),
    hit_rate_vs_6040 = mean(beat_6040, na.rm = TRUE),
    hit_rate_vs_aw = mean(beat_aw, na.rm = TRUE),
    avg_alpha_vs_spy = mean(strategy_ret_net - benchmark_spy_ret, na.rm = TRUE),
    avg_alpha_vs_6040 = mean(strategy_ret_net - benchmark_6040_ret, na.rm = TRUE),
    avg_alpha_vs_aw = mean(strategy_ret_net - benchmark_allweather_ret, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(months))

write_csv(regime_hit_rates, "regime_hit_rates_step20.csv")