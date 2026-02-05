
_____________________
CURRENTLY WORKING ON:
---------------------
Reviewing/Polishing Current Projects and Looking for a new dataset to work with. Excel will be used as well. 




__________________
PORTFOLIO PROJECTS
------------------
Working on the projects i have uploaded here have been equally challenging and rewarding as i am still learning as i progress. The repositories featured here represent both academic and independently conducted projects. While some analyses were completed as part of formal coursework and followed predefined guidelines, others were designed and executed independently to better reflect real-world analytical workflows. These projects focus on understanding the business question, preparing and querying data, defining meaningful KPIs, and communicating insights clearly through written analysis and visualisation.

_
PROJECT 1: UCI RETAIL UK ( R & RMD)
-
This project analyses transactional data from the UCI Online Retail dataset, filtered to UK operations, to understand how customer behaviour and product performance impact revenue sustainability and operational risk. The central business question is  “How do customer engagement patterns, purchase frequency, returns, and product-level performance influence the overall economic value and operational risk in an online retail environment?” The analysis is structured to directly address this question through clearly defined KPIs and a reproducible R workflow documented in R Markdown.


The key performance indicators guiding the analysis include Customer Lifetime Value, purchase frequency and repeat rate, return exposure, net revenue by product and category, and revenue volatility. Each KPI is explicitly calculated from the cleaned transactional dataset, allowing the project to quantify both customer contribution and risk. Customer-level metrics are derived from transactional aggregation, supporting cohort analysis and k-means clustering to identify distinct behavioural personas. Product and category performance metrics, adjusted for returns, provide insight into revenue concentration, risk exposure, and stability over time. Sequential purchasing patterns are also examined to evaluate whether repeat customer behaviour exhibits predictable temporal regularity or is primarily opportunistic.


The workflow begins by transforming raw Excel data into a clean, analysis-ready UK dataset, addressing missing values, duplicates, inconsistent formatting, and extreme values. Subsequent R scripts generate all relevant metrics, tables, and visualizations, integrating them within the R Markdown report to ensure transparency and reproducibility. The final outputs include a cleaned datasets, customer and product summaries, cohort retention tables, persona clusters, and sequential purchase analyses. Collectively, they demonstrate how the KPIs answer the business question, providing actionable insights into customer retention, revenue strategy, and operational risk management in UK online retail.

_
PROJECT 2: LONDON TUBE PERFORMANCE (SQLITE+ WORD+ TABLEAU)
-
This project analyses London Underground line-level performance using Excess Journey Time (EJT) data to evaluate how reliably different lines deliver service over time. The core business question is “which Underground lines perform best based on excess journey time, how does performance evolve over time, and how consistent is service delivery across the network?” The analysis focuses on comparative performance rather than incident-level disruption, using line-by-month aggregates to support network-wide benchmarking.

The analytical framework is built around Transport for London’s  performance indicators. Average excess journey time is used to measure overall line performance, with lower values indicating better service. Temporal variation is assessed through monthly trends, while operational reliability is captured through delay variability. The latter is calculated as the dispersion of excess journey time across months for each line. Together, these KPIs allow performance to be evaluated not only in terms of speed, but also consistency and stability.



The workflow is implemented entirely in SQLite and documented for portfolio use, with outputs designed for direct consumption in Tableau. The analysis begins with a structured data quality audit to confirm dataset grain, temporal coverage, completeness of critical fields, numeric validity, and metadata stability. Instead of using an aggressive approach to data cleaning, extreme but valid values are deliberately preserved to be used as  indicators of operational stress rather than noise. Feature engineering introduces business-readable delay metrics and a calendar-based time variable to support ranking and time-series visualisation, without altering the underlying monthly, line-level structure of the data.


KPI computation translates validated performance data into comparable metrics across all lines. Average delay and delay variability are calculated directly from excess journey time, producing defensible rankings that distinguish between lines that are fast but volatile and those that deliver slower yet more consistent service.Monthly line-level delay outputs are exported as CSV files and subsequently used in Tableau to visualise performance rankings and trends over time, ensuring a clear separation between data preparation, KPI logic, and visual analysis.


AI assistance, namely “ChatGPT”  was used as a supporting tool to review and refine SQL logic, validate KPI definitions, and improve the structure of analytical queries. 


