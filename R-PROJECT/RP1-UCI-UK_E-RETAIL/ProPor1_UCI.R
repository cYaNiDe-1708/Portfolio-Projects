# ================================
# MASTER SETUP — RUN ONCE
# ================================

required_packages <- c(
  "readxl","readr","dplyr","janitor","lubridate","stringr",
  "ggplot2","factoextra","cluster","tidyr","scales","ggrepel"
)

missing <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing)) install.packages(missing, dependencies = TRUE)

invisible(lapply(required_packages, library, character.only = TRUE))


# ================================
# PHASE 1 — DATA INTAKE & QUALITY CONTROL
# ================================

# STEP 1: LOAD DATA
df <- read_excel("Online Retail.xlsx")
write.csv(df, "O_R.csv", row.names = FALSE, fileEncoding = "UTF-8")

# STEP 2: CLEAN COLUMN NAMES & REMOVE DUPLICATES
initial_rows <- nrow(df)
df <- df %>%
  clean_names() %>%
  distinct()
duplicates_removed <- initial_rows - nrow(df)

# STEP 3: FIX DATA TYPES
df <- df %>%
  mutate(
    invoice_date = as.POSIXct(invoice_date, format = "%Y-%m-%d %H:%M:%S"),
    quantity = as.numeric(quantity),
    unit_price = as.numeric(unit_price),
    customer_id = as.character(customer_id)
  )

# STEP 4: HANDLE MISSING VALUES
df <- df %>%
  filter(!is.na(description)) %>%
  mutate(missing_customerID = is.na(customer_id))

# STEP 5: CLEAN TEXT FIELDS & FILTER TO UK ONLY
df <- df %>%
  mutate(
    description = str_squish(description) %>% str_to_title(),
    country = str_squish(country) %>% str_to_title()
  ) %>%
  mutate(country = recode(country,
                          "U.K." = "UK",
                          "United Kingdom" = "UK")) %>%
  filter(country == "UK")  # keep UK only

# STEP 6: REMOVE OUTLIERS IN UNIT PRICE
Q1 <- quantile(df$unit_price, 0.25, na.rm = TRUE)
Q3 <- quantile(df$unit_price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
upper <- Q3 + 1.5 * IQR
lower <- Q1 - 1.5 * IQR
df <- df %>% filter(unit_price >= lower & unit_price <= upper)

# STEP 7: CREATE TRANSACTION VALUE & RETURN FLAG
df <- df %>%
  mutate(
    transaction_value = quantity * unit_price,
    return_flag = ifelse(quantity < 0, 1, 0)
  )

# STEP 8: SEPARATE SALES AND RETURNS
sales_df <- df %>% filter(quantity > 0)
returns_df <- df %>% filter(quantity < 0)

# STEP 9: EXPORT CLEANED DATA
write.csv(df, "cleaned_online_retail_uk.csv", row.names = FALSE)
write.csv(sales_df, "sales_only_uk.csv", row.names = FALSE)
write.csv(returns_df, "returns_only_uk.csv", row.names = FALSE)

# STEP 10: DATA QUALITY SUMMARY
data_quality <- tibble(
  total_rows = nrow(df),
  missing_customerID = sum(is.na(df$customer_id)),
  missing_description = sum(is.na(df$description)),
  negative_quantity = sum(df$quantity < 0),
  zero_or_negative_price = sum(df$unit_price <= 0),
  duplicates_removed = duplicates_removed
)
write.csv(data_quality, "data_quality_summary_uk.csv", row.names = FALSE)


# ================================
# PHASE 2 — CUSTOMER LIFECYCLE & PERSONA CLUSTERING
# ================================

# STEP 1: CREATE CUSTOMER FEATURES
customer_features <- sales_df %>%
  group_by(customer_id) %>%
  summarise(
    first_purchase = min(invoice_date),
    last_purchase = max(invoice_date),
    active_days = as.numeric(difftime(max(invoice_date), min(invoice_date), units = "days")),
    avg_days_between = ifelse(
      n_distinct(invoice_date) > 1,
      mean(as.numeric(diff(sort(invoice_date))), na.rm = TRUE),
      0
    ),
    total_invoices = n_distinct(invoice_no),
    total_spend = sum(transaction_value),
    avg_basket_size = mean(transaction_value),
    return_ratio = mean(quantity < 0),
    .groups = "drop"
  )

# STEP 2: CREATE COHORTS BY FIRST PURCHASE MONTH
customer_features <- customer_features %>%
  mutate(first_purchase_month = floor_date(first_purchase, "month"))

cohort_data <- sales_df %>%
  left_join(customer_features %>% select(customer_id, first_purchase_month), by = "customer_id") %>%
  mutate(purchase_month = floor_date(invoice_date, "month")) %>%
  group_by(first_purchase_month, purchase_month) %>%
  summarise(active_customers = n_distinct(customer_id), .groups = "drop")

cohort_matrix <- cohort_data %>%
  pivot_wider(names_from = purchase_month, values_from = active_customers, values_fill = 0)

# STEP 3: COHORT RETENTION VISUALIZATION
ggplot(cohort_data, aes(x = purchase_month, y = active_customers, color = first_purchase_month)) +
  geom_line() +
  labs(title = "Customer Cohort Retention Over Time (UK)", x = "Purchase Month", y = "Active Customers") +
  theme_minimal()

# STEP 4: CUSTOMER PERSONA CLUSTERING
cluster_data <- customer_features %>%
  select(total_spend, total_invoices, avg_basket_size, return_ratio, active_days) %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .)))

zero_var_cols <- sapply(cluster_data, function(x) sd(x) == 0)
if(any(zero_var_cols)) cluster_data <- cluster_data[, !zero_var_cols]

cluster_scaled <- scale(cluster_data)
set.seed(123)
kmeans_res <- kmeans(cluster_scaled, centers = 4, nstart = 25)
customer_features$persona <- as.factor(kmeans_res$cluster)

# STEP 5: PERSONA CLUSTER VISUALIZATION
fviz_cluster(kmeans_res, data = cluster_scaled,
             geom = "point", ellipse.type = "convex",
             main = "Customer Persona Clusters (UK)")

# STEP 6: BUSINESS INTERPRETATION TABLE
persona_summary <- customer_features %>%
  group_by(persona) %>%
  summarise(
    n_customers = n(),
    avg_spend = mean(total_spend),
    avg_invoices = mean(total_invoices),
    avg_basket = mean(avg_basket_size),
    avg_return_ratio = mean(return_ratio),
    avg_active_days = mean(active_days),
    .groups = "drop"
  )

# STEP 7: CUSTOMER FEATURES SUMMARY

# Ensure correct types
customer_features$first_purchase <- as.Date(customer_features$first_purchase)
customer_features$last_purchase <- as.Date(customer_features$last_purchase)

# Calculate summary statistics
summary_stats <- customer_features %>%
  summarise(
    avg_active_days = mean(active_days, na.rm = TRUE),
    median_active_days = median(active_days, na.rm = TRUE),
    avg_total_invoices = mean(total_invoices, na.rm = TRUE),
    median_total_invoices = median(total_invoices, na.rm = TRUE),
    avg_total_spend = mean(total_spend, na.rm = TRUE),
    median_total_spend = median(total_spend, na.rm = TRUE),
    avg_basket_size = mean(avg_basket_size, na.rm = TRUE),
    median_basket_size = median(avg_basket_size, na.rm = TRUE),
    avg_days_between = mean(avg_days_between, na.rm = TRUE),
    median_days_between = median(avg_days_between, na.rm = TRUE),
    avg_return_ratio = mean(return_ratio, na.rm = TRUE),
    max_return_ratio = max(return_ratio, na.rm = TRUE)
  )

summary_stats






# ================================
# PHASE 3 — PRODUCT TAXONOMY & RETURN-ADJUSTED PERFORMANCE
# ================================

# STEP 1: CLEAN PRODUCT DESCRIPTIONS + ASSIGN PRODUCT CATEGORIES + MERGE CUSTOMER PERSONA
sales_df <- sales_df %>%
  mutate(description_clean = description %>%
           str_to_lower() %>%
           str_replace_all("[[:punct:]]", " ") %>%
           str_squish() %>%
           str_replace_all("\\b[0-9]{2,}\\b", ""))
# ASSIGN PRODUCT CATEGORIES + MERGE CUSTOMER PERSONA
sales_df <- sales_df %>%
  mutate(product_category = case_when(
    str_detect(description_clean, "gift|present|card") ~ "Gifts",
    str_detect(description_clean, "kitchen|cook|utensil") ~ "Kitchenware",
    str_detect(description_clean, "home|decor|cushion|lamp") ~ "Home Décor",
    str_detect(description_clean, "pen|notebook|stationery|diary") ~ "Stationery",
    str_detect(description_clean, "christmas|holiday|seasonal") ~ "Seasonal",
    str_detect(description_clean, "bag|scarf|hat|accessory") ~ "Accessories",
    TRUE ~ "Other"
  ))
# MERGE CUSTOMER PERSONA
merged_view <- sales_df %>%
  left_join(customer_features %>% select(customer_id, persona), by = "customer_id")



# STEP 2: PRODUCT METRICS + PRODUCT METRICS SUMMARY
product_metrics <- merged_view %>%
  group_by(stock_code, description_clean, product_category) %>%
  summarise(
    gross_revenue = sum(transaction_value[quantity > 0], na.rm = TRUE),
    refunded_revenue = abs(sum(transaction_value[quantity < 0], na.rm = TRUE)),
    net_revenue = gross_revenue - refunded_revenue,
    return_rate = ifelse(gross_revenue > 0, refunded_revenue / gross_revenue, 0),
    revenue_volatility = sd(transaction_value, na.rm = TRUE),
    total_quantity = sum(quantity),
    .groups = "drop"
  )
# PRODUCT METRICS SUMMARY
product_metrics_summary <- product_metrics %>%
  summarise(
    avg_gross_revenue = mean(gross_revenue, na.rm = TRUE),
    median_gross_revenue = median(gross_revenue, na.rm = TRUE),
    avg_refunded_revenue = mean(refunded_revenue, na.rm = TRUE),
    median_refunded_revenue = median(refunded_revenue, na.rm = TRUE),
    avg_net_revenue = mean(net_revenue, na.rm = TRUE),
    median_net_revenue = median(net_revenue, na.rm = TRUE),
    avg_return_rate = mean(return_rate, na.rm = TRUE),
    max_return_rate = max(return_rate, na.rm = TRUE),
    avg_revenue_volatility = mean(revenue_volatility, na.rm = TRUE),
    max_revenue_volatility = max(revenue_volatility, na.rm = TRUE),
    avg_total_quantity = mean(total_quantity, na.rm = TRUE),
    max_total_quantity = max(total_quantity, na.rm = TRUE)
  )
product_metrics_summary

# STEP 3: CATEGORY METRICS
category_metrics <- product_metrics %>%
  group_by(product_category) %>%
  summarise(
    total_gross_revenue = sum(gross_revenue),
    total_refunded_revenue = sum(refunded_revenue),
    total_net_revenue = sum(net_revenue),
    avg_return_rate = mean(return_rate),
    avg_revenue_volatility = mean(revenue_volatility, na.rm = TRUE),
    .groups = "drop"
  )

# STEP 4: VISUALIZATION & INTERPRETATION
#CATEGORY NET REVENUE
ggplot(category_metrics, aes(x = reorder(product_category, total_net_revenue), y = total_net_revenue)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Net Revenue by Product Category (UK)", x = "Product Category", y = "Net Revenue (£)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

#MONTHLY REVENUE DATA
monthly_rev <- merged_view %>%
  mutate(month = floor_date(invoice_date, "month")) %>%
  group_by(month, product_category) %>%
  summarise(monthly_revenue = sum(transaction_value, na.rm = TRUE), .groups = "drop")

#PRODUCT RISK-REWARD MAP
ggplot(monthly_rev, aes(x = month, y = product_category, fill = monthly_revenue)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(
    title = "Monthly Revenue Heatmap by Category (UK)",
    x = "Month",
    y = "Category",
    fill = "Revenue (£)"
  ) +
  theme_minimal()

#PERSONA PURCHASE PATTERNS BY CATEGORY
persona_pref <- merged_view %>%
  group_by(persona, product_category) %>%
  summarise(total_spend = sum(transaction_value, na.rm = TRUE), .groups = "drop")
ggplot(persona_pref, aes(x = persona, y = product_category, fill = total_spend)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Purchase Patterns by Persona and Category (UK)", x = "Persona Cluster", y = "Product Category", fill = "£ Spend") +
  theme_minimal()


# ================================
# PHASE 4 — LIMITED SEQUENTIAL SIGNALS
# ================================

# STEP 1: CONSTRUCT AND QUALIFY CUSTOMER PURCHASE SEQUENCES
repeat_customers <- sales_df %>%
  arrange(customer_id, invoice_date) %>%
  group_by(customer_id) %>%
  mutate(
    purchase_order = row_number(),
    next_purchase_gap_days = as.numeric(
      difftime(lead(invoice_date), invoice_date, units = "days")
    )
  ) %>%
  filter(n() >= 3) %>%   # qualify to true repeat customers
  ungroup()

# STEP 2: SEQUENTIAL STABILITY METRICS
# Summarise inter-purchase behaviour across qualified repeat customers

sequential_signal <- repeat_customers %>%
  summarise(
    median_gap_days = median(next_purchase_gap_days, na.rm = TRUE),
    gap_iqr = IQR(next_purchase_gap_days, na.rm = TRUE),
    pct_consistent_gaps = mean(next_purchase_gap_days <= 30, na.rm = TRUE)
  )

# STEP 3: BUSINESS INSIGHT TABLE (EXPORT)
# Executive-facing interpretation of sequential behaviour

phase4_insight <- tibble(
  interpretation = "Sequential behaviour exists only in narrow, episodic pockets.",
  implication = "Retail demand is predominantly opportunistic rather than time-based.",
  actionability = "Restrict automation to immediate post-purchase windows."
)

write.csv(
  phase4_insight,
  "phase4_sequential_insight_uk.csv",
  row.names = FALSE
)


