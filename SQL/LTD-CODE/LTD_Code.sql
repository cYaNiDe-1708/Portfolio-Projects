/* DISCLAIMER: IF RE-RUN "ERROR: DUPLICATE COLUMN NAME: DELAY_MINUTES" WILL APPEAR. 
PLEASE RUN THE CODE STEP BY STEP WHILE THE ISSUE IS BEING FIXED*/ 

-- PHASE 1: DATASET UNDERSTANDING AND DATA QUALITY AUDIT 

-- Row count and temporal coverage
SELECT
    COUNT(*) AS total_rows,
    MIN(Month) AS first_month,
    MAX(Month) AS last_month
FROM LTD;
/* Assesses dataset size and confirms historical span */

-- Line coverage and distribution
SELECT
    Line,
    COUNT(*) AS months_covered
FROM LTD
GROUP BY Line
ORDER BY months_covered DESC;
/* Confirms uniform coverage across lines for fair comparison */

-- Missing value audit (critical fields only)
SELECT
    SUM(CASE WHEN Line IS NULL THEN 1 ELSE 0 END) AS null_line,
    SUM(CASE WHEN Month IS NULL THEN 1 ELSE 0 END) AS null_month,
    SUM(CASE WHEN Excess IS NULL THEN 1 ELSE 0 END) AS null_excess,
    SUM(CASE WHEN Scheduled IS NULL THEN 1 ELSE 0 END) AS null_scheduled,
    SUM(CASE WHEN TOTAL IS NULL THEN 1 ELSE 0 END) AS null_total
FROM LTD;
/* Confirms analytical fields are complete */

-- Numeric range validation – Excess Journey Time
SELECT
    MIN(Excess) AS min_excess,
    MAX(Excess) AS max_excess,
    ROUND(AVG(Excess), 2) AS avg_excess
FROM LTD;
/* Establishes baseline expectations for delay values */

-- Scheduled vs actual journey time sanity check
SELECT
    MIN(Scheduled) AS min_scheduled,
    MAX(Scheduled) AS max_scheduled,
    MIN(TOTAL) AS min_total,
    MAX(TOTAL) AS max_total
FROM LTD;
/* Confirms TOTAL ≈ Scheduled + Excess */

-- Detect anomalous excess values
SELECT *
FROM LTD
WHERE Excess < 0
   OR Excess > 5 * (SELECT AVG(Excess) FROM LTD);
/* Flags extreme disruption periods without removing them */

-- Metadata consistency check
SELECT
    Line,
    COUNT(DISTINCT Length) AS length_variants,
    COUNT(DISTINCT Stations) AS station_variants,
    COUNT(DISTINCT Opened) AS opened_variants,
    COUNT(DISTINCT Type) AS type_variants
FROM LTD
GROUP BY Line;
/* Confirms metadata stability for dimensional analysis */

-- Final audit snapshot
SELECT *
FROM LTD
LIMIT 10;
/* Confirms presence of nulls only in derived fields */












-- PHASE 2: DATA CLEANING & FEATURE ENGINEERING 

-- Validate performance metric logic
SELECT
    MIN(TOTAL - (Scheduled + Excess)) AS min_diff,
    MAX(TOTAL - (Scheduled + Excess)) AS max_diff
FROM LTD;
/* Confirms Excess can safely represent delay */

-- Populate business-readable delay metric
UPDATE LTD
SET delay_minutes = Excess
WHERE delay_minutes IS NULL;
/* Creates a clear KPI field for analysis and visualization */

-- Create month_date column for proper calendar timeline
ALTER TABLE LTD ADD COLUMN month_date DATE;
/* Converts numeric month index to calendar month for visuals */

-- Populate month_date based on first month
UPDATE LTD
SET month_date = DATE('2015-01-01', '+' || (Month - 1) || ' months');
/* Ensures X-axis in Tableau shows real dates */

-- Peak/off-peak handling
UPDATE LTD
SET is_peak = 0
WHERE is_peak IS NULL;
/* Avoids false precision given monthly, line-level grain */

-- Validate derived fields
SELECT
    SUM(CASE WHEN delay_minutes IS NULL THEN 1 ELSE 0 END) AS null_delay_minutes,
    SUM(CASE WHEN is_peak IS NULL THEN 1 ELSE 0 END) AS null_is_peak
FROM LTD;
/* Confirms no remaining gaps */

-- Analytical snapshot
SELECT
    Line,
    Month,
    month_date,
    delay_minutes,
    is_peak
FROM LTD
LIMIT 10;
/* Verifies readiness for KPI computation */










-- PHASE 3: KPI COMPUTATION 

-- KPI 1: Average delay per line & ranking 
SELECT
    Line,
    ROUND(AVG(delay_minutes), 2) AS avg_delay_minutes,
    RANK() OVER (ORDER BY AVG(delay_minutes)) AS performance_rank
FROM LTD
GROUP BY Line
ORDER BY performance_rank;

-- KPI 2: Delay variability (SQLite-compatible std dev)
SELECT
    Line,
    ROUND(AVG(delay_minutes), 2) AS avg_delay_minutes,
    ROUND(
        SQRT(
            AVG(delay_minutes * delay_minutes)
            - AVG(delay_minutes) * AVG(delay_minutes)
        ), 2
    ) AS delay_variability
FROM LTD
GROUP BY Line
ORDER BY delay_variability;
/* Measures operational reliability */

-- Monthly Trend View For Time-Series Visualisation KPI 2
DROP VIEW IF EXISTS line_month_trends;
CREATE VIEW line_month_trends AS
SELECT
    Line,
    month_date AS Month,
    delay_minutes
FROM LTD
ORDER BY Line, month_date;

-- EXPORT KPI 2
SELECT
    Line,
    month_date AS Month,
    delay_minutes
FROM LTD
ORDER BY Line, month_date;



