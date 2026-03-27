DROP VIEW IF EXISTS macro_features;

CREATE VIEW macro_features AS
SELECT
    date,
    cpi_yoy,
    LAG(cpi_yoy, 1) OVER (ORDER BY date) AS cpi_yoy_lag1,
    unrate,
    LAG(unrate, 1) OVER (ORDER BY date) AS unrate_lag1,
    fedfunds,
    gs10,
    tb3ms,
    (gs10 - tb3ms) AS term_spread,
    LAG(gs10 - tb3ms, 1) OVER (ORDER BY date) AS term_spread_lag1
FROM macro_data;