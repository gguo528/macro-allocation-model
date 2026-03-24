# =========================================================
# V2 Step 1: Build continuous macro + momentum signal set
# Purpose:
# - reuse V1 macro features
# - convert them into comparable continuous signals
# - add forward-looking market momentum inputs
# =========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(quantmod)
  library(xts)
})

Sys.setlocale("LC_TIME", "C")

# ---------------------------------------------------------
# Helper: rolling z-score using trailing window
# Uses only information available up to time t
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# Momentum helper
# 12-1 momentum:
# cumulative return from t-12 to t-1
# Excludes most recent month to reduce short-term reversal effects
# ---------------------------------------------------------
calc_12_1_mom <- function(ret) {
  lag(
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

# ---------------------------------------------------------
# Reuse V1 macro table
# Assumes macro_monthly already exists from V1
# ---------------------------------------------------------
v2_macro_base <- macro_monthly %>%
  arrange(date) %>%
  select(
    date,
    
    # Inflation
    cpi_yoy,
    core_cpi_yoy,
    cpi_3m_ann,
    core_cpi_3m_ann,
    cpi_yoy_6m_delta_smooth,
    core_cpi_yoy_6m_delta_smooth,
    
    # Growth / labor
    unrate_6m_delta_smooth,
    payems_3m_ann_smooth,
    indpro_3m_ann_smooth,
    houst_yoy_smooth,
    umcsent_6m_delta,
    
    # Policy / curve / credit
    real_fedfunds,
    real_10y,
    term_spread_10y3m,
    term_spread_10y2y,
    front_end_spread_2y3m,
    baa_aaa_spread,
    baa_10y_spread_smooth
  )

# ---------------------------------------------------------
# Build continuous macro z-scores
# Sign convention:
# Positive = more risk-friendly unless explicitly inflationary
# Inflation remains its own block for later mapping
# ---------------------------------------------------------
v2_macro_signals <- v2_macro_base %>%
  mutate(
    # -------------------------
    # Inflation block
    # Higher = hotter inflation
    # -------------------------
    z_cpi_yoy               = roll_zscore(cpi_yoy, 36),
    z_core_cpi_yoy          = roll_zscore(core_cpi_yoy, 36),
    z_cpi_3m_ann            = roll_zscore(cpi_3m_ann, 36),
    z_core_cpi_3m_ann       = roll_zscore(core_cpi_3m_ann, 36),
    z_cpi_yoy_6m_delta      = roll_zscore(cpi_yoy_6m_delta_smooth, 36),
    z_core_cpi_yoy_6m_delta = roll_zscore(core_cpi_yoy_6m_delta_smooth, 36),
    
    # -------------------------
    # Growth block
    # For unemployment delta:
    # falling unemployment is good, so invert sign
    # -------------------------
    z_unrate_trend     = -roll_zscore(unrate_6m_delta_smooth, 36),
    z_payems_growth    = roll_zscore(payems_3m_ann_smooth, 36),
    z_indpro_growth    = roll_zscore(indpro_3m_ann_smooth, 36),
    z_housing_growth   = roll_zscore(houst_yoy_smooth, 36),
    z_sentiment_change = roll_zscore(umcsent_6m_delta, 36),
    
    # -------------------------
    # Policy block
    # Higher real rates = more restrictive, so invert for risk
    # -------------------------
    z_real_fedfunds_risk = -roll_zscore(real_fedfunds, 36),
    z_real_10y_risk      = -roll_zscore(real_10y, 36),
    
    # -------------------------
    # Curve block
    # Steeper / more normal curve = more risk-friendly
    # -------------------------
    z_curve_10y3m = roll_zscore(term_spread_10y3m, 36),
    z_curve_10y2y = roll_zscore(term_spread_10y2y, 36),
    z_curve_2y3m  = roll_zscore(front_end_spread_2y3m, 36),
    
    # -------------------------
    # Credit block
    # Wider spreads = worse, so invert sign
    # -------------------------
    z_baa_aaa_risk = -roll_zscore(baa_aaa_spread, 36),
    z_baa_10y_risk = -roll_zscore(baa_10y_spread_smooth, 36)
  ) %>%
  arrange(date)

# ---------------------------------------------------------
# Reuse V1 asset return table
# Assumes asset_returns already exists from V1 Step 3
# ---------------------------------------------------------
v2_momentum_signals <- asset_returns %>%
  arrange(date) %>%
  mutate(
    # Single-asset momentum
    mom_spy_12_1 = calc_12_1_mom(spy_ret),
    mom_qqq_12_1 = calc_12_1_mom(qqq_ret),
    mom_iwm_12_1 = calc_12_1_mom(iwm_ret),
    mom_efa_12_1 = calc_12_1_mom(efa_ret),
    mom_eem_12_1 = calc_12_1_mom(eem_ret),
    mom_ief_12_1 = calc_12_1_mom(ief_ret),
    mom_tlt_12_1 = calc_12_1_mom(tlt_ret),
    mom_lqd_12_1 = calc_12_1_mom(lqd_ret),
    mom_hyg_12_1 = calc_12_1_mom(hyg_ret),
    mom_tip_12_1 = calc_12_1_mom(tip_ret),
    mom_gld_12_1 = calc_12_1_mom(gld_ret),
    mom_dbc_12_1 = calc_12_1_mom(dbc_ret),
    mom_vnq_12_1 = calc_12_1_mom(vnq_ret),
    mom_bil_12_1 = calc_12_1_mom(bil_ret)
  ) %>%
  mutate(
    # Sleeve-level momentum
    mom_us_equity_12_1 = rowMeans(
      across(c(mom_spy_12_1, mom_qqq_12_1, mom_iwm_12_1)),
      na.rm = TRUE
    ),
    
    mom_intl_equity_12_1 = rowMeans(
      across(c(mom_efa_12_1, mom_eem_12_1)),
      na.rm = TRUE
    ),
    
    mom_treasury_12_1 = rowMeans(
      across(c(mom_ief_12_1, mom_tlt_12_1)),
      na.rm = TRUE
    ),
    
    mom_credit_12_1 = rowMeans(
      across(c(mom_lqd_12_1, mom_hyg_12_1)),
      na.rm = TRUE
    ),
    
    mom_real_assets_12_1 = rowMeans(
      across(c(mom_tip_12_1, mom_gld_12_1, mom_dbc_12_1, mom_vnq_12_1)),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    across(starts_with("mom_"), ~na_if(., NaN))
  ) %>%
  select(date, starts_with("mom_")) %>%
  arrange(date)

# ---------------------------------------------------------
# Join macro and momentum signals into V2 signal table
# Also create standardized momentum measures
# ---------------------------------------------------------
v2_signals <- v2_macro_signals %>%
  left_join(v2_momentum_signals, by = "date") %>%
  mutate(
    z_mom_us_equity   = roll_zscore(mom_us_equity_12_1, 36),
    z_mom_intl_equity = roll_zscore(mom_intl_equity_12_1, 36),
    z_mom_treasury    = roll_zscore(mom_treasury_12_1, 36),
    z_mom_credit      = roll_zscore(mom_credit_12_1, 36),
    z_mom_real_assets = roll_zscore(mom_real_assets_12_1, 36),
    
    # Broad risk appetite momentum signal
    z_mom_risk = rowMeans(
      across(c(z_mom_us_equity, z_mom_intl_equity, z_mom_credit)),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    across(starts_with("z_mom_"), ~na_if(., NaN))
  ) %>%
  arrange(date)

# ---------------------------------------------------------
# Keep a compact working table for V2
# This is the clean input to V2 Step 2
# ---------------------------------------------------------
v2_signal_table <- v2_signals %>%
  select(
    date,
    
    # Inflation z-signals
    z_cpi_yoy,
    z_core_cpi_yoy,
    z_cpi_3m_ann,
    z_core_cpi_3m_ann,
    z_cpi_yoy_6m_delta,
    z_core_cpi_yoy_6m_delta,
    
    # Growth z-signals
    z_unrate_trend,
    z_payems_growth,
    z_indpro_growth,
    z_housing_growth,
    z_sentiment_change,
    
    # Policy / curve / credit z-signals
    z_real_fedfunds_risk,
    z_real_10y_risk,
    z_curve_10y3m,
    z_curve_10y2y,
    z_curve_2y3m,
    z_baa_aaa_risk,
    z_baa_10y_risk,
    
    # Momentum
    z_mom_us_equity,
    z_mom_intl_equity,
    z_mom_treasury,
    z_mom_credit,
    z_mom_real_assets,
    z_mom_risk
  ) %>%
  arrange(date)

# ---------------------------------------------------------
# Inspect / save
# ---------------------------------------------------------
write_csv(v2_signal_table, "v2_signal_table_step1.csv")

v2_signal_table_clean <- v2_signal_table %>%
  filter(if_all(-date, ~ !is.na(.))) %>%
  arrange(date)

# =========================================================
# V2 Step 2: Build composite macro + momentum score
# Structure:
# - Equal weight within each block
# - Equal weight across blocks
# - Lag signals by 1 month for execution
# =========================================================

# ---------------------------------------------------------
# Use cleaned signal table from Step 1
# ---------------------------------------------------------
v2_step2 <- v2_signal_table_clean %>%
  arrange(date) %>%
  mutate(
    
    # =====================================================
    # 1. Inflation block (higher = hotter inflation)
    # =====================================================
    inflation_score = rowMeans(
      across(c(
        z_cpi_yoy,
        z_core_cpi_yoy,
        z_cpi_3m_ann,
        z_core_cpi_3m_ann,
        z_cpi_yoy_6m_delta,
        z_core_cpi_yoy_6m_delta
      )),
      na.rm = TRUE
    ),
    
    # =====================================================
    # 2. Growth block (higher = stronger growth)
    # =====================================================
    growth_score = rowMeans(
      across(c(
        z_unrate_trend,
        z_payems_growth,
        z_indpro_growth,
        z_housing_growth,
        z_sentiment_change
      )),
      na.rm = TRUE
    ),
    
    # =====================================================
    # 3. Policy / liquidity block (higher = easier conditions)
    # =====================================================
    policy_score = rowMeans(
      across(c(
        z_real_fedfunds_risk,
        z_real_10y_risk,
        z_curve_10y3m,
        z_curve_10y2y,
        z_curve_2y3m,
        z_baa_aaa_risk,
        z_baa_10y_risk
      )),
      na.rm = TRUE
    ),
    
    # =====================================================
    # 4. Risk / momentum block (higher = risk-on confirmation)
    # =====================================================
    risk_score = rowMeans(
      across(c(
        z_mom_us_equity,
        z_mom_intl_equity,
        z_mom_credit,
        z_mom_real_assets
      )),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    across(
      c(inflation_score, growth_score, policy_score, risk_score),
      ~na_if(., NaN)
    )
  ) %>%
  mutate(
    
    # =====================================================
    # 5. Composite macro score
    # Equal weight across 4 blocks
    # =====================================================
    composite_score_raw = rowMeans(
      across(c(
        inflation_score,
        growth_score,
        policy_score,
        risk_score
      )),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    
    # Optional normalization (keeps scale stable over time)
    composite_score_z = rollapplyr(
      composite_score_raw,
      width = 36,
      FUN = function(x) {
        s <- sd(x, na.rm = TRUE)
        m <- mean(x, na.rm = TRUE)
        last_x <- tail(x, 1)
        
        if (is.na(s) || s == 0) {
          NA_real_
        } else {
          (last_x - m) / s
        }
      },
      fill = NA,
      partial = FALSE
    )
  ) %>%
  mutate(
    
    # =====================================================
    # 6. Execution signal (lagged)
    # Prevents lookahead bias
    # =====================================================
    signal_for_allocation = lag(composite_score_z, 1)
  ) %>%
  arrange(date)

# ---------------------------------------------------------
# Clean final usable dataset for allocation
# ---------------------------------------------------------
v2_step2_clean <- v2_step2 %>%
  filter(!is.na(signal_for_allocation)) %>%
  arrange(date)

# ---------------------------------------------------------
# Inspect
# ---------------------------------------------------------

v2_step2_clean %>%
  select(
    date,
    inflation_score,
    growth_score,
    policy_score,
    risk_score,
    composite_score_z,
    signal_for_allocation
  ) %>%
  slice_head(n = 10)

v2_step2_clean %>%
  slice_tail(n = 10)

# ---------------------------------------------------------
# Save
# ---------------------------------------------------------
write_csv(v2_step2_clean, "v2_step2_scores.csv")

# =========================================================
# V2 Step 3: Map composite score -> dynamic portfolio weights
# Improved version:
# - rank-based scaling
# - convex allocation
# - momentum override
# - more dispersion between risk-on and risk-off states
# =========================================================

df <- v2_step2_clean %>%
  arrange(date)

clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

v2_weights <- df %>%
  arrange(date) %>%
  mutate(
    # -----------------------------------------------------
    # 1. Separate macro and momentum components
    # -----------------------------------------------------
    macro_score = rowMeans(
      across(c(inflation_score, growth_score, policy_score)),
      na.rm = TRUE
    ),
    momentum_score = risk_score,
    
    # -----------------------------------------------------
    # 2. Rank-based scaling
    # Expands signal distribution to [0, 1]
    # -----------------------------------------------------
    macro_rank = percent_rank(macro_score),
    momentum_rank = percent_rank(momentum_score),
    
    # -----------------------------------------------------
    # 3. Combine macro + momentum
    # -----------------------------------------------------
    blended_rank = 0.3 * macro_rank + 0.7 * momentum_rank,
    
    # -----------------------------------------------------
    # 4. Convex mapping
    # -----------------------------------------------------
    risk_budget_base = if_else(
      blended_rank > 0.5,
      blended_rank^3,
      blended_rank^2 * 0.5
    ),
    
    # -----------------------------------------------------
    # 5. Initial momentum override using broad risk momentum
    # -----------------------------------------------------
    risk_budget_adj = case_when(
      z_mom_risk < -0.5 ~ risk_budget_base * 0.45,
      z_mom_risk <  0.0 ~ risk_budget_base * 0.70,
      TRUE              ~ risk_budget_base
    )
  ) %>%
  mutate(
    # -----------------------------------------------------
    # 5b. Extra equity-trend override
    # -----------------------------------------------------
    risk_budget_adj = if_else(
      z_mom_us_equity < 0,
      risk_budget_adj * 0.4,
      risk_budget_adj
    ),
    
    # -----------------------------------------------------
    # 6. Floor / ceiling
    # -----------------------------------------------------
    w_risk_total = clamp(risk_budget_adj, 0.15, 0.95),
    w_def_total  = 1 - w_risk_total
  ) %>%
  mutate(
    # -----------------------------------------------------
    # 7. Split within risk bucket
    # Make composition responsive to inflation and momentum
    # -----------------------------------------------------
    
    # Equity share inside risk bucket
    equity_share_raw = 0.70 + 0.25 * momentum_rank - 0.10 * pmax(inflation_score, 0),
    equity_share = clamp(equity_share_raw, 0.35, 0.75),
    
    # Credit share inside risk bucket
    credit_share_raw = 0.12 + 0.03 * momentum_rank,
    credit_share = clamp(credit_share_raw, 0.10, 0.30),
    
    # Real asset share inside risk bucket
    real_asset_share_raw = 1 - equity_share - credit_share,
    real_asset_share = clamp(real_asset_share_raw, 0.10, 0.35),
    
    # Renormalize risk sleeves
    risk_share_sum = equity_share + credit_share + real_asset_share,
    
    equity_share_n = equity_share / risk_share_sum,
    credit_share_n = credit_share / risk_share_sum,
    real_asset_share_n = real_asset_share / risk_share_sum
  ) %>%
  mutate(
    # -----------------------------------------------------
    # 8. Split equity sleeve into US / Intl
    # -----------------------------------------------------
    us_share_raw = 0.70 + 0.10 * momentum_rank,
    us_share = clamp(us_share_raw, 0.60, 0.85),
    intl_share = 1 - us_share
  ) %>%
  mutate(
    # -----------------------------------------------------
    # 9. Split defensive sleeve into Treasuries / Cash
    # -----------------------------------------------------
    treasury_share_raw = 0.75 + 0.10 * (1 - momentum_rank),
    treasury_share = clamp(treasury_share_raw, 0.70, 0.90),
    cash_share = 1 - treasury_share
  ) %>%
  mutate(
    # --------------------
    w_equity_total = w_risk_total * equity_share_n,
    
    w_credit       = w_risk_total * credit_share_n,
    w_real_assets  = w_risk_total * real_asset_share_n,
    
    w_us_equity    = w_equity_total * us_share,
    w_intl_equity  = w_equity_total * intl_share,
    
    w_treasury     = w_def_total * treasury_share,
    w_cash         = w_def_total * cash_share
  ) %>%
  mutate(
    # -----------------------------------------------------
    # 11. Check sum
    # -----------------------------------------------------
    w_total =
      w_us_equity +
      w_intl_equity +
      w_credit +
      w_real_assets +
      w_treasury +
      w_cash
  ) %>%
  arrange(date)

# Checks
v2_weights %>%
  summarise(
    min_total = min(w_total, na.rm = TRUE),
    max_total = max(w_total, na.rm = TRUE)
  )

v2_weights %>%
  summarise(
    min_risk = min(w_risk_total, na.rm = TRUE),
    p25_risk = quantile(w_risk_total, 0.25, na.rm = TRUE),
    median_risk = median(w_risk_total, na.rm = TRUE),
    p75_risk = quantile(w_risk_total, 0.75, na.rm = TRUE),
    max_risk = max(w_risk_total, na.rm = TRUE)
  )

v2_weights %>%
  select(
    date,
    macro_score,
    momentum_score,
    blended_rank,
    z_mom_risk,
    w_risk_total,
    w_us_equity,
    w_intl_equity,
    w_credit,
    w_real_assets,
    w_treasury,
    w_cash
  ) %>%
  slice_head(n = 10)

write_csv(v2_weights, "v2_weights_step3_improved.csv")

# =========================================================
# V2 Step 4: Compute portfolio returns
# Improved version:
# - uses improved sleeve weights
# - computes gross and net returns
# - same benchmark structure can be reused in Step 5
# =========================================================

# ---------------------------------------------------------
# 1. Map sleeve returns from V1 asset_returns
# ---------------------------------------------------------
v2_returns_base <- asset_returns %>%
  arrange(date) %>%
  transmute(
    date,
    
    # US equity sleeve
    us_equity =
      0.50 * spy_ret +
      0.30 * qqq_ret +
      0.20 * iwm_ret,
    
    # International equity sleeve
    intl_equity =
      0.60 * efa_ret +
      0.40 * eem_ret,
    
    # Credit sleeve
    credit =
      0.60 * lqd_ret +
      0.40 * hyg_ret,
    
    # Real asset sleeve
    real_assets =
      0.25 * tip_ret +
      0.25 * gld_ret +
      0.25 * dbc_ret +
      0.25 * vnq_ret,
    
    # Treasury sleeve
    treasury =
      0.50 * ief_ret +
      0.50 * tlt_ret,
    
    # Cash sleeve
    cash = bil_ret
  )

# ---------------------------------------------------------
# 2. Join weights with returns
# ---------------------------------------------------------
v2_portfolio <- v2_weights %>%
  select(
    date,
    w_us_equity,
    w_intl_equity,
    w_credit,
    w_real_assets,
    w_treasury,
    w_cash
  ) %>%
  left_join(v2_returns_base, by = "date") %>%
  arrange(date)

# ---------------------------------------------------------
# 3. Compute gross portfolio return
# ---------------------------------------------------------
v2_portfolio <- v2_portfolio %>%
  mutate(
    port_ret_gross =
      w_us_equity   * us_equity +
      w_intl_equity * intl_equity +
      w_credit      * credit +
      w_real_assets * real_assets +
      w_treasury    * treasury +
      w_cash        * cash
  )

# ---------------------------------------------------------
# 4. Transaction costs
# Use half-turnover convention to avoid double counting
# ---------------------------------------------------------
tc_per_turnover <- 0.001

v2_portfolio <- v2_portfolio %>%
  arrange(date) %>%
  mutate(
    turnover =
      0.5 * (
        abs(w_us_equity   - lag(w_us_equity)) +
          abs(w_intl_equity - lag(w_intl_equity)) +
          abs(w_credit      - lag(w_credit)) +
          abs(w_real_assets - lag(w_real_assets)) +
          abs(w_treasury    - lag(w_treasury)) +
          abs(w_cash        - lag(w_cash))
      ),
    turnover = replace_na(turnover, 0),
    tc_cost = turnover * tc_per_turnover,
    port_ret_net = port_ret_gross - tc_cost
  )

# ---------------------------------------------------------
# 5. Clean final dataset
# ---------------------------------------------------------
v2_portfolio_clean <- v2_portfolio %>%
  filter(!is.na(port_ret_net)) %>%
  arrange(date)

# ---------------------------------------------------------
# 6. Inspect / save
# ---------------------------------------------------------

v2_portfolio_clean %>%
  select(date, port_ret_gross, port_ret_net, turnover, tc_cost) %>%
  slice_head(n = 10)

v2_portfolio_clean %>%
  slice_tail(n = 10)

write_csv(v2_portfolio_clean, "v2_portfolio_returns_improved.csv")

# =========================================================
# V2 Step 5: Performance evaluation
# Structure:
# - Compute cumulative returns
# - Risk/return metrics
# - Benchmark comparison (SPY, 60/40, All Weather)
# =========================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ---------------------------------------------------------
# 1. Prepare strategy returns
# ---------------------------------------------------------
strategy <- v2_portfolio_clean %>%
  select(date, port_ret_gross, port_ret_net) %>%
  arrange(date)

# ---------------------------------------------------------
# 2. Build benchmark returns (same as V1)
# ---------------------------------------------------------
benchmarks <- asset_returns %>%
  arrange(date) %>%
  transmute(
    date,
    
    # SPY
    SPY = spy_ret,
    
    # 60/40 (stocks/bonds)
    `60_40` =
      0.6 * spy_ret +
      0.4 * (0.5 * ief_ret + 0.5 * tlt_ret),
    
    # All Weather (simple proxy)
    AllWeather =
      0.3 * spy_ret +
      0.4 * (0.5 * ief_ret + 0.5 * tlt_ret) +
      0.15 * gld_ret +
      0.15 * dbc_ret
  )

# ---------------------------------------------------------
# 3. Merge
# ---------------------------------------------------------
perf_df <- strategy %>%
  left_join(benchmarks, by = "date") %>%
  drop_na()

# ---------------------------------------------------------
# 4. Cumulative returns
# ---------------------------------------------------------
perf_df <- perf_df %>%
  mutate(
    strat_gross_cum = cumprod(1 + port_ret_gross),
    strat_net_cum   = cumprod(1 + port_ret_net),
    spy_cum         = cumprod(1 + SPY),
    bal6040_cum     = cumprod(1 + `60_40`),
    aw_cum          = cumprod(1 + AllWeather)
  )

# ---------------------------------------------------------
# 5. Performance metrics function
# ---------------------------------------------------------
perf_stats <- function(ret) {
  n <- length(ret)
  ann_ret <- prod(1 + ret)^(12 / n) - 1
  ann_vol <- sd(ret) * sqrt(12)
  
  sharpe <- ifelse(ann_vol == 0, NA, ann_ret / ann_vol)
  
  downside <- sd(ret[ret < 0]) * sqrt(12)
  sortino <- ifelse(downside == 0, NA, ann_ret / downside)
  
  cum <- cumprod(1 + ret)
  drawdown <- cum / cummax(cum) - 1
  max_dd <- min(drawdown, na.rm = TRUE)
  
  calmar <- ifelse(max_dd == 0, NA, ann_ret / abs(max_dd))
  
  hit_rate <- mean(ret > 0)
  
  tibble(
    Months = n,
    CAGR = ann_ret,
    AnnVol = ann_vol,
    Sharpe = sharpe,
    Sortino = sortino,
    Calmar = calmar,
    MaxDrawdown = max_dd,
    HitRate = hit_rate,
    BestMonth = max(ret, na.rm = TRUE),
    WorstMonth = min(ret, na.rm = TRUE)
  )
}

# ---------------------------------------------------------
# 6. Compute metrics
# ---------------------------------------------------------
v2_summary <- bind_rows(
  perf_stats(perf_df$port_ret_gross) %>% mutate(series = "V2_Strategy_Gross"),
  perf_stats(perf_df$port_ret_net)   %>% mutate(series = "V2_Strategy_Net"),
  perf_stats(perf_df$SPY)            %>% mutate(series = "SPY"),
  perf_stats(perf_df$`60_40`)        %>% mutate(series = "60_40"),
  perf_stats(perf_df$AllWeather)     %>% mutate(series = "AllWeather")
) %>%
  select(series, everything())

# ---------------------------------------------------------
# 7. Print results
# ---------------------------------------------------------
print(v2_summary)

# ---------------------------------------------------------
# 8. Optional: Save cumulative series for charting
# ---------------------------------------------------------
cum_df <- perf_df %>%
  select(
    date,
    strat_gross_cum,
    strat_net_cum,
    spy_cum,
    bal6040_cum,
    aw_cum
  )

write_csv(cum_df, "v2_cumulative_returns.csv")
write_csv(v2_summary, "v2_performance_summary.csv")
```