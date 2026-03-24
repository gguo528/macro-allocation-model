# Macro Allocation Model

## Overview

This project implements a macro-driven asset allocation framework that dynamically adjusts portfolio exposure based on economic conditions.

Two models are developed:

- V1: Discrete regime-based allocation  
- V2: Continuous scoring model with momentum integration  

The objective is to test whether macroeconomic signals can improve risk-adjusted returns relative to static benchmarks.

---

## Data

### Macro Inputs (FRED)

- CPI (headline and core)
- Unemployment rate
- Federal Funds rate
- Treasury yields (2Y, 10Y, 3M)
- Payrolls
- Industrial production
- Housing starts
- Consumer sentiment
- Corporate credit spreads (AAA, BAA)

### Asset Universe (Yahoo Finance)

- Equities: SPY, QQQ, IWM, EFA, EEM  
- Fixed Income: IEF, TLT, LQD, HYG, TIP  
- Real Assets: GLD, DBC, VNQ  
- Cash Proxy: BIL  

---

## Model Structure

### V1: Regime Model

- Macro variables are transformed into:
  - Growth
  - Inflation
  - Policy signals
- Each period is assigned to a discrete regime
- Fixed portfolio weights are mapped to each regime
- Monthly rebalancing

---

### V2: Continuous Model

- Macro signals converted to standardized scores
- Combined with 12–1 momentum signals
- Portfolio weights determined using:
  - Rank-based scaling
  - Convex risk allocation
  - Momentum overrides
- Produces smoother, more adaptive positioning

---

## Backtest Details

- Frequency: Monthly  
- Start: 2004-01  
- Rebalancing: Monthly  
- Transaction costs included  
- Benchmarks:
  - SPY (100% equity)
  - 60/40 portfolio  

---

## Results

The models are evaluated on:

- CAGR  
- Volatility  
- Sharpe ratio  
- Max drawdown  
- Turnover  
- Subperiod stability  

Key findings:

- V1 captures broad regime shifts but is coarse  
- V2 improves adaptability but is sensitive to signal construction  
- Momentum integration materially impacts performance  

---

## Repository Structure

macro-allocation-model 
├── README.md 
├── macro_model.pdf 
├── R 
│ ├── helpers.R 
│ ├── v1_model.R 
│ └── v2_model.R 
├── data 
└── outputs

---

## How to Run

1. Clone the repository

```bash
git clone https://github.com/gguo528/macro-allocation-model.git
```
2. Open in RStudio

3. Run:
source("R/v1_model.R")
source("R/v2_model.R")

4. Or knit the report:

rmarkdown::render("Macro-portfolio-model.Rmd")

Notes

- FRED data is pulled using fredgraph.csv endpoints (no API key required)

- All time series are aligned to month-end

- Signals are lagged to avoid lookahead bias

Limitations

- Model is sensitive to:
  - signal specification
  - weighting scheme
  - transaction costs
- No optimization or machine learning used
- Results are historical and not predictive

Author
George Guo
