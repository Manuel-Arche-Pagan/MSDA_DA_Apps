# initializing data + environment
packages <- c('car', 'tidyverse', 'data.table', 'caret', 'pROC', 'rpart', 'rpart.plot', 'randomForest', 'broom', 'readxl')
lapply(packages, library, character.only = TRUE)

df1 <- read_excel('online_retail.xlsx',
                 sheet = 1)
df2 <- read_excel('online_retail.xlsx',
                  sheet = 2)
df <- bind_rows(df1, df2)


# checking missing values
colSums(is.na(df))
# there are 4382 missing descriptions and 24007 missing customer IDs


# missing descriptions
# Do missing descriptions correspond with stock codes that have known descriptions in other rows?
miss_by_code <- df %>%
  group_by(StockCode) %>%
  summarise(
    n = n(),
    n_missing = sum(is.na(Description)),
    pct_missing = (n_missing / n) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_missing), desc(n))

miss_by_code #


always_missing_codes <- miss_by_code %>%
  filter(pct_missing == 100) %>%
  pull(StockCode)

always_missing_codes # there are 355 stock codes with no known description


sometimes_missing_codes <- miss_by_code %>%
  filter(pct_missing < 100) %>%
  pull(StockCode)

sometimes_missing_codes 

# impute most-common description into rows with sometimes_missing_codes
# lookup table
desc_lookup <- df %>%
  filter(StockCode %in% sometimes_missing_codes, !is.na(Description)) %>%
  count(StockCode, Description, sort = TRUE) %>%
  group_by(StockCode) %>%
  slice_max(n, with_ties = FALSE) %>%
  ungroup() %>%
  select(StockCode, Description_impute = Description)

# join lookup and fill only missing descriptions
df_imputed <- df %>%
  left_join(desc_lookup, by = "StockCode") %>%
  mutate(Description = coalesce(Description, Description_impute)) %>%
  select(-Description_impute)

colSums(is.na(df_imputed)) # now there are only 363 rows with missing descriptions

# imputing descriptions for stock codes with no known description
df_imputed2 <- df_imputed %>%
  mutate(Description = if_else(
    is.na(Description),
    paste0("Unknown (StockCode ", StockCode, ")"),
    Description
  ))

colSums(is.na(df_imputed2)) # no missing descriptions
# description imputation complete





# missing customer ID
# checking that each invoice is associated with only one customerID
invoice_check <- df %>%
  filter(!is.na('Customer ID')) %>%
  group_by(Invoice) %>%
  summarise(n_customers = n_distinct('Customer ID'),
            .groups = "drop") %>%
  filter(n_customers > 1)

invoice_check 
# returns 0 rows, meaning each invoice is associated with only 1 customerID

# imputing customer ID where Invoice value exists in another row where customer ID exists
df_imputed3 <- df_imputed2 %>%
  group_by(Invoice) %>%
  fill(`Customer ID`, .direction = "downup") %>%
  ungroup()

colSums(is.na(df_imputed3))
# there are still over 240,000 rows with no customer ID

df_missingid <- df_imputed3[is.na(df_imputed3$'Customer ID'),] %>%
  group_by(Invoice)

df_missingid %>%
  distinct(Invoice)

# are missing customers concentrated in certain countries?
missingbycountry <- df_imputed3 %>%
  mutate(missing_id = is.na(`Customer ID`)) %>%
  group_by(Country, missing_id) %>%
  summarise(n = n()) %>%
  group_by(Country) %>%
  mutate(prop = n / sum(n))

missingcountries <- missingbycountry[missingbycountry$prop != 1,]
# there are no countries that contain no customer ID at all, however there are 12 countries with some proportion of null id


df_imputed3 %>%
  filter(is.na(`Customer ID`)) %>%
  count(Invoice) %>%
  summary()
# median number of items for an invoice without customer id is 1, but mean is 27 and max is 1350

# before removing null customer ids from a customer dataset, we should see if null customer ids make up a large proportion of revenue
# if null customer id rows make up a large portion of revenue, we must report the impact of removing them
df_imputed3 %>%
  mutate(missing_id = is.na(`Customer ID`),
         revenue = Quantity * Price) %>%
  group_by(missing_id) %>%
  summarise(
    pct_transactions = n() / nrow(df_imputed3),
    pct_revenue = sum(revenue) / sum(df_imputed3$Quantity * df_imputed3$Price)
  )
# rows with null customer id make up 22.8% of observations and 13.7% of total revenue

# we can remove null customer ids for customer-level analysis, but we will keep null customer ids for aggregate demand analysis

# check for duped rows
print(sum(duplicated(df_imputed3))) # there are over 34,000 duped rows

# remove dupes
df3 <- df_imputed3 %>%
  distinct()

nrow(df_imputed3) # 1067317
nrow(df3) # 1033036



# dataset for product/sales calculations
df_all <- df3

# dataset for customer analytics (no null customerIDs)
df_cust <- df3 %>%
  filter(!is.na(`Customer ID`))

######################################### end data cleaning










######################################### Product & Sales Analysis

## What are the top-selling products? Which products are declining in popularity?
## Are there seasonal purchase patterns (e.g., holiday spikes)?
## Can we forecast demand for key products to improve stock management?

### What are the top selling products? 
#Decided on using units as the measurement. Revenue is still below if it needs to be brought up. 
library(dplyr)

#Calculates the total units sold per product 
top_products_units <- df_products |> 
  filter(Quantity > 0, Price > 0) |>   # keep real sales only
  group_by(Description) |> 
  summarise(total_units = sum(Quantity), .groups = "drop") |> 
  arrange(desc(total_units))

#Gets the top 10 products by units sold 
top10_units <- top_products_units |> 
  slice_max(total_units, n = 10)

#Plots the top 10 products (By units) 
ggplot(top10_units, aes(x = reorder(Description, total_units), y = total_units)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = comma(total_units)),
            hjust = 1.1,
            color = "white",
            size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Top 10 Selling Products by Units Sold",
       x = "Product",
       y = "Total Units Sold") +
  theme_minimal()






#Total revenue per product. Which products generate the most sales over time 
library(dplyr)

top_products <- df_all |> 
  filter(Quantity > 0, Price > 0) |> #keeps only the valid sales we need
  mutate(revenue = Quantity * Price) |> #this creates the new "revenue"
  group_by(Description) |> # treats each prodcut seperately so all of the same products are grouped togehter. 
  summarise(total_revenue = sum(revenue), .groups = "drop") |> # adds up the revenue and removes the grouping after 
  arrange(desc(total_revenue)) #top selling products appear first

top10 <- top_products |> 
  slice_max(total_revenue, n=10)

library(ggplot2)
library(scales)
ggplot (top10, aes(x=reorder(Description, total_revenue),total_revenue))+
  geom_col(fill = "darkgreen") +
  geom_text(aes(label=dollar(total_revenue)),
            hjust = 1.1, 
            color = "white",
            size = 3.5)+
  coord_flip()+
  scale_y_continuous(labels = dollar,
                     expand=expansion(mult = c(0,0.1))) +
  labs(title = "Tope 10 Selling Products by Revenue",
       x="Product", y="Total Revenue")

### There seemed to be a lot of "Manual" sales. So I am going to check if the manual descriptions mean something else by comparing them to the stock code to find out if there is a code that connects to another description which will lead to another product 
manual_rows <- df_all |> 
  filter(tolower(Description) == "manual")
manual_rows

manual_codes <- manual_rows |> #confirmed that the stock codes associated with manual are M and m and not other products
  distinct(StockCode)
manual_codes

df_all |> #looked at the Quantity and noticed there was large returns or corrections and massive bulk entries. Not normal behavior 
  filter(StockCode %in% c("M", "m")) %>%
  summary()

df_all |> # Looked at the max and min price and noticed it is not a consistent inventory item 
  filter(StockCode %in% c("M", "m")) %>%
  summarise(
    min_price = min(Price, na.rm=TRUE),
    max_price = max(Price, na.rm=TRUE),
    avg_price = mean(Price, na.rm=TRUE)
    )

# the top selling products by revenue confirmed "Manual" is inflating the rankings 
# decided to take it out just for revenue sake and deciding the top selling products. But might still be important later. 
# could not find no connection to other products. The stock code are directly tied to manual and the pricing has not consistency to find a connection

df_products <- df_all |> 
  filter(!(StockCode %in% c("M","m")))

top_products <- df_products |>  #Recalculated revenue per product 
  filter(Quantity > 0, Price > 0) %>%   # keep real sales only
  mutate(revenue = Quantity * Price) |> 
  group_by(Description) |> 
  summarise(total_revenue = sum(revenue), .groups = "drop") |> 
  arrange(desc(total_revenue))

top10 <- top_products |> 
  slice_max(total_revenue, n = 10)



ggplot (top10, aes(x=reorder(Description, total_revenue),total_revenue))+
  geom_col(fill = "darkgreen") +
  geom_text(aes(label=dollar(total_revenue)),
            hjust = 1.1, 
            color = "white",
            size = 3.5)+
  coord_flip()+
  scale_y_continuous(labels = dollar,
                     expand=expansion(mult = c(0,0.1))) +
  labs(title = "Tope 10 Selling Products by Revenue",
       x="Product", y="Total Revenue")




## Are there seasonal purchase patterns (e.g., holiday spikes)?

df_products <- df_products |> # Added Revenue variable 
  mutate(revenue = Quantity * Price)

library(dplyr)
library(lubridate)

monthly_revenue <- df_products |> # Created monthly revenue table 
  filter(Quantity > 0, Price > 0) |>   # keep real sales
  mutate(month = floor_date(as.Date(InvoiceDate), "month")) |> 
  group_by(month) |> 
  summarise(total_revenue = sum(revenue), .groups = "drop")

ggplot(monthly_revenue, aes(x = month, y = total_revenue)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = dollar) +
  scale_x_date(date_labels = "%b %Y",
              date_breaks = "2 months") +
  labs(title = "Monthly Revenue Over Time",
       x = "Month",
       y = "Total Revenue ($)") +
  theme_minimal()











## Can we forecast demand for key products to improve stock management?

key_product <- "ASSORTED COLOUR BIRD ORNAMENT" #This is on of the top product but can be done for the others as needed 

product_monthly_units <- df_products |> 
  filter(Description == key_product, #kept only rows for the product and positive quantity's as a safety measure 
         Quantity > 0) |> 
  mutate(month = floor_date(as.Date(InvoiceDate), "month")) |> #Converted the transaction dates into months 
  group_by(month) |> 
  summarise(units_sold = sum(Quantity), .groups = "drop") |> 
  arrange(month)


ts_data <- ts(product_monthly_units$units_sold,#created the time series object 
              start = c(year(min(product_monthly_units$month)),
                        month(min(product_monthly_units$month))),
              frequency = 12)

#install.packages("forecast")
library(forecast)

model <- auto.arima(ts_data) #finds the best ARIMA model 
forecast_values <- forecast(model, h = 6) #forecasted the next 6 months 

autoplot(forecast_values) +
  labs(title = paste("6-Month Demand Forecast for", key_product),
       y = "Units Sold",
       x = "Time")
##The dark purple is 80% confidence level and the light purple is 95% confidence level
##The model has such a wide range since there is only two years of data and not 


### Which products are declining in popularity?
# Revenue trend over time. Ended up changing it to units to try to see a real decline and not monetary values 
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)

#Created monthly units sold for each product
prod_month_units <- df_products |>
  filter(Quantity > 0, Price > 0) |>
  mutate(
    month = floor_date(as.Date(InvoiceDate), "month")
  ) |>
  group_by(Description, month) |>
  summarise(monthly_units = sum(Quantity), .groups = "drop") |>
  arrange(Description, month)

# Created numeric time variable for regression
prod_month_units <- prod_month_units |>
  mutate(t = as.numeric(month))

# Kept products with enough history and recent activity
last_month <- max(prod_month_units$month)

eligible <- prod_month_units |>
  group_by(Description) |>
  summarise(
    months_present = n(),
    last_seen = max(month),
    .groups = "drop"
  ) |>
  filter(
    months_present >= 18,
    last_seen >= last_month - months(3)
  )

prod_month_units_filtered <- prod_month_units |>
  semi_join(eligible, by = "Description")

# Fit a linear trend for each product
trend_table_units <- prod_month_units_filtered |>
  nest(data = -Description) |>
  mutate(
    model = map(data, ~ lm(monthly_units ~ t, data = .x)),
    results = map(model, tidy)
  ) |>
  unnest(results) |>
  filter(term == "t") |>
  select(Description, slope = estimate, p.value) |>
  arrange(slope)

# Selected the top 10 declining products
declining_products_units <- trend_table_units |>
  filter(slope < 0) |>
  slice_min(slope, n = 10)

# Kept products with a statistically menaingful negative trend 
declining_products_units <- trend_table_units |>
  filter(slope < 0, p.value < 0.05) |>
  slice_min(slope, n = 10)

# Saved product names for plotting
decline_names_units <- declining_products_units$Description

# Plotted monthly units sold with regression line
prod_month_units_filtered |>
  filter(Description %in% decline_names_units) |>
  ggplot(aes(x = month, y = monthly_units)) +
  geom_line(color = "red", linewidth = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ Description, scales = "free_y") +
  labs(
    title = "Top Declining Products (Units Sold Trend)",
    x = "Month",
    y = "Monthly Units Sold"
  ) +
  theme_minimal()









#How can pricing strategies be adjusted to increase basket value?

#We could build a market  basket analysis for items frequently bought together in the same transaction.
#install.packages("widyr")
library(widyr)

#Creates product pairs so we can see if they are worth bundling together 
pair_counts <- df_products |> 
  filter(Quantity > 0, Price > 0) |> 
  mutate(item = str_squish(toupper(Description))) |> # capitalized everything so that there are no missing products and removed the extra spacing if there is any between words 
  group_by(Invoice) |> # grouped each transaction by invoice 
  summarise(item = list(unique(item)), .groups="drop") |> # created a list of products per invoice and ensured that duplicates didnt count twice
  unnest(item) |> # turned them into rows with the same invoice
  pairwise_count(item, Invoice, sort = TRUE) # counts how often two products appear in the same invoice 

# plots the product pairs
pair_counts |> 
  head(10) |> 
  mutate(pair = paste(item1, "&", item2)) |> 
  ggplot(aes(x = reorder(pair, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Co-purchase count",
    title = "Top Purchased Product Pairs"
  )

#This pairwise product analysis is used to identify items that are frequently purchased together within the same transaction
#By grouping the purchases by invioce and counting co-occurring product pairs we identify combinations of items that customers commonly buy together 
#This allows for the opportunity for product bundling and cross-selling strategies that can increase basket value 

# red and white heart t lights are commonly bought together
# discount the red lights when white ones are bought, and vice-versa, could lead to increased basket values

######################################################## end product analysis














######################################################## Customer Analysis

# using dataset without null values in customer ID for accurate customer analytics
df_cust_prep <- df_cust %>%
  rename(CustomerID = 'Customer ID') %>% # renaming customer id variable
  mutate(obs_revenue = Quantity * Price)

# example problem invoice
df_cust_prep %>%
  filter(Invoice == 489560) %>%
  arrange(StockCode) %>%
  print(n=31)
# the issue here is that each purchased product is supposed to be listed ONCE per invoice, with quantity denoting how many were purchased
# in some invoices like Invoice # 489560, the same product is listed on multiple rows with different quantities for unknown reasons
# it will be assumed that the true quantity of product purchased is equal to the sum of quantities of both rows
# for example, if "REX CASH+CARRY JUMBO SHOPPER" is listed twice with respective quantities 3 and 1, we assume 4 were purchased

# shows how many unique products each customer purchased
sku_summary_per_customer <- df_cust_prep %>%
  group_by(CustomerID) %>%
  summarise(
    total_unique_skus = n_distinct(StockCode),
    .groups = "drop"
  )

# combining by invoice number and creating features like invoice_revenue and returns_flag
invoice_summary <- df_cust_prep %>%
  group_by(Invoice) %>%
  summarise(
    CustomerID = first(CustomerID),
    InvoiceDate = min(InvoiceDate),
    Country = first(Country),
    invoice_revenue = sum(obs_revenue),
    invoice_items = sum(Quantity),
    unique_skus = n_distinct(StockCode),
    returns_flag = any(Quantity < 0)
  ) %>%
  ungroup()


# creating customer dataset
customer_df <- invoice_summary %>%
  filter(!is.na(CustomerID)) %>%
  group_by(CustomerID) %>%
  left_join(sku_summary_per_customer, by = 'CustomerID') %>%
  arrange(InvoiceDate) %>%
  summarise(
    total_revenue = sum(invoice_revenue, na.rm = TRUE),
    n_orders = n(),
    avg_order_value = round(total_revenue / n_orders, 2),
    first_order_date = min(InvoiceDate),
    last_order_date = max(InvoiceDate),
    recency_days = round(as.numeric(difftime(max(df_cust_prep$InvoiceDate), last_order_date)), 2), # days since last purchase (from last day in data)
    tenure_days = round(as.numeric(difftime(last_order_date, first_order_date, units = 'days')), 2), # days between first and last order
    invoices_w_returns = sum(returns_flag),
    prop_invoices_returned = round(invoices_w_returns / n_orders, 2),
    avg_items_per_invoice = round(mean(invoice_items, na.rm = TRUE), 2),
    total_unique_skus = first(total_unique_skus),
    country = names(sort(table(Country), decreasing = TRUE))[1]
  ) %>%
  ungroup()
# there are 5942 known customers


# SKU-level and product-concentration metrics using transaction-level data
sku_metrics <- df_cust_prep %>%
  filter(!is.na(CustomerID)) %>%
  group_by(CustomerID, StockCode) %>%
  summarise(sku_revenue = sum(obs_revenue, na.rm = TRUE), sku_quantity = sum(Quantity, na.rm = TRUE)) %>%
  ungroup()


# interpurchase data such as mean and median days between orders
interpurchase <- invoice_summary %>%
  filter(!is.na(CustomerID)) %>%
  arrange(CustomerID, InvoiceDate) %>%
  group_by(CustomerID) %>%
  summarise(
    interpurchase_days_mean = {
      days <- as.numeric(difftime(lead(InvoiceDate), InvoiceDate, units = "days"))
      days <- days[!is.na(days)]
      if (length(days) == 0) NA_real_ else round(mean(days, na.rm = TRUE), 2)
    },
    interpurchase_days_median = {
      days <- as.numeric(difftime(lead(InvoiceDate), InvoiceDate, units = "days"))
      days <- days[!is.na(days)]
      if (length(days) == 0) NA_real_ else round(median(days, na.rm = TRUE), 2)
    }
  ) %>%
  ungroup()


# months active and orders per month
months_active <- invoice_summary %>%
  filter(!is.na(CustomerID)) %>%
  mutate(year_month = format(InvoiceDate, "%Y-%m")) %>%
  group_by(CustomerID) %>%
  summarise(
    months_active = n_distinct(year_month),
    orders_per_month = round(n() / months_active, 2)
  ) %>%
  ungroup()


# seasonality behavior
seasonality <- invoice_summary %>%
  filter(!is.na(CustomerID)) %>%
  mutate(month = month(InvoiceDate)) %>%
  group_by(CustomerID, month) %>%
  summarise(cnt = n()) %>%
  slice_max(cnt, with_ties = FALSE) %>%
  ungroup() %>%
  select(CustomerID, preferred_month = month)



# combine dataframes
customer_features <- customer_df %>%
  left_join(interpurchase, by = "CustomerID") %>%
  left_join(months_active, by = "CustomerID") %>%
  left_join(seasonality, by = "CustomerID") %>%
  # additional features
  mutate(
    avg_monthly_revenue = if_else(months_active > 0, round(total_revenue / months_active, 2), NA_real_),
    avg_order_value = if_else(n_orders > 0, round(total_revenue / n_orders, 2), NA_real_),
    repeat_flag = n_orders > 1
  )

### (end customer feature engineering) 


### which customers generate most revenue?
# simple approach: using total_revenue in customer features to determine top x%
top_revenue_customers <- customer_features %>%
  arrange(desc(total_revenue)) %>%
  select(CustomerID, total_revenue, n_orders, avg_order_value) %>%
  head(20)


# RFM approach
# each component -- recency, frequency, and monetary value -- scored 1-5 by creating 5 equally-sized bins for each relevant metric
customer_rfm <- customer_features %>%
  mutate(
    r_score = ntile(-recency_days, 5), # negative recency_days inverts the bins so that more recent (fewer days) = higher score
    f_score = ntile(n_orders, 5),
    m_score = ntile(total_revenue, 5),
    rfm_score = paste0(r_score, f_score, m_score),
    rfm_total = r_score + f_score + m_score
  )

# now we can determine highest value customers to target for retention (all rfm components 4 or 5)
hv_customers <- customer_rfm %>%
  filter(r_score >=4 & f_score >=4 & m_score >=4)

# check to see if high rfm corresponds with high revenue
rfm_revenues <- customer_rfm %>%
  group_by(rfm_total) %>%
  summarise(
    avg_revenue = mean(total_revenue),
    median_revenue = median(total_revenue),
    n_customers = n()
  ) %>%
  arrange(desc(rfm_total))
# avg and median revenue are highly correlated with rfm_total
# side note: distribution of number of customers at each rfm_total value are quite consistent

ggplot(data = rfm_revenues, aes(x = rfm_total, y = median_revenue)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_x_continuous(breaks = c(3:15)) +
  labs(title = 'Median Revenue Generated at each RFM Total Score',
       x = 'RFM Total Score (r_score + f_score + m_score)',
       y = 'Median Revenue Generated') +
  theme_minimal()


# how much revenue is generated for each rfm_total segment?
rfm_rev_sum <- customer_rfm %>%
  group_by(rfm_total) %>%
  summarise(
    segment_revenue = sum(total_revenue),
    segment_share = segment_revenue / sum(customer_rfm$total_revenue)
  ) %>%
  arrange(desc(segment_share))

ggplot(data = rfm_rev_sum, aes(x = rfm_total, y = segment_revenue)) +
  geom_col() +
  scale_x_continuous(breaks = c(3:15)) +
  labs(title = 'Total Revenue Generated per RFM Total Segment',
       x = 'RFM Total Score (r_score + f_score + m_score)',
       y = 'Total Revenue Generated')


# using rfm score to segment customers into groups
# Champions: high R, high F, high M (top revenue drivers)
# at-risk high value: low R, high F, high M (big frequent spenders who haven't purchased in a while)
# big occasional spender: high R, low F, high M (spends lots but infrequently)
# loyal low-spenders: high R, high F, low M (spends not a lot of money often)
# new customers: high R, low F, low M
# low engagement: low R, low F, low M
# regular customers: mid-tier customers with scores above 2 and 3 who don't belong in any other segment

customer_rfm <- customer_rfm %>%
  mutate(segment = case_when(
    r_score >= 4 & f_score >= 4 & m_score >= 4 ~ "Champion",
    r_score <= 2 & f_score >= 4 & m_score >= 4 ~ "At-risk High Value",
    r_score >= 4 & f_score <= 2 & m_score >= 4 ~ "Big Occasional Spender",
    r_score >= 4 & f_score >= 4 & m_score <= 2 ~ "Loyal Low Spender",
    r_score >= 4 & f_score <= 2 & m_score <= 2 ~ "New Customer",
    r_score <= 2 & f_score <= 2 & m_score <= 2 ~ "Low Engagement",
    r_score >= 3 & f_score >= 2 & m_score >= 2 ~ "Regular Customers", # mid-tier customers
    TRUE ~ "No Segment"
  ))

rfm_summary <- customer_rfm %>%
  group_by(segment) %>%
  summarise(segment_freq = n())

# how much revenue does each segment generate?
segment_rev <- customer_rfm %>%
  group_by(segment) %>%
  summarise(revenue = sum(total_revenue)) %>%
  mutate(prop_rev = round(revenue / sum(revenue), 4))

# checking calculation
segment_rev %>%
  summarise(sum <- sum(prop_rev))

# visualizations of segment revenue
ggplot(data = segment_rev,
       aes(x = reorder(segment, revenue),
           y = revenue,
           fill = segment)) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(revenue)),
    vjust = -0.2,
    size = 3.5
  ) +
  labs(title = "Revenue Generated per Customer Segment",
       x = "Segment",
       y = "Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = 'none')

# Champions are dominating the chart
# same thing -Champions segment
ggplot(data = segment_rev[-3,],
       aes(x = reorder(segment, revenue),
           y = revenue,
           fill = segment)) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(revenue)),
    vjust = -0.2,
    size = 3.5
  ) +
  labs(title = "Revenue Generated per Customer Segment (minus Champions)",
       x = "Segment",
       y = "Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = 'none')


# proportion of revenue generated per segment
ggplot(data = segment_rev,
       aes(x = reorder(segment, prop_rev),
           y = prop_rev * 100,
           fill = segment)) +
  geom_col() +
  geom_text(
    aes(label = paste0(prop_rev * 100, '%')),
    vjust = -0.2,
    size = 3.5
  ) +
  labs(title = "Percent Contribution of Revenue per Customer Segment",
       x = "Segment",
       y = "Revenue Percent Contribution") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = 'none')


# visualization of segments
ggplot(data = rfm_summary,
       aes(x = reorder(segment, segment_freq),
           y = segment_freq,
           fill = segment)) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(segment_freq)),
    vjust = -0.2,
    size = 3.5
  ) +
  labs(title = "Segmentation Frequencies of All Customers",
       x = "Segment",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none")

# how many customers fit into each segment?
ggplot(data = top_bottom_rev,
       aes(x = reorder(Country, total_revenue, decreasing = TRUE),
           y = total_revenue,
           fill = group)) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(total_revenue, accuracy = 0.01)),
    vjust = -0.2,
    size = 2.5
  ) +
  labs(title = "Top and Bottom Countries by Total Revenue Generated",
       x = "Country",
       y = "Total Revenue",
       fill = "Group") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




################################################ country sales growth tracking

# using full data to track country sales
df_country_prep <- df_all %>%
  rename(CustomerID = 'Customer ID') %>%
  mutate(
    revenue = Quantity * Price,
    year_month = floor_date(InvoiceDate, 'month') # all days are now the first of the month for standardization
  )

# tracks revenue and number of sales per country per month
country_month_df <- df_country_prep %>%
  group_by(Country, year_month) %>%
  summarise(
    total_revenue = sum(revenue, na.rm = TRUE),
    total_orders = n_distinct(Invoice),
    total_customers = n_distinct(CustomerID), 
    avg_order_value = round(total_revenue / total_orders, 2)
  ) %>%
  arrange(Country, year_month) %>%
  ungroup()

# creating month-over-month statistics
country_month_df <- country_month_df %>%
  group_by(Country) %>%
  arrange(year_month) %>%
  mutate(
    revenue_lag = lag(total_revenue),
    mom_growth = round((total_revenue - revenue_lag) / revenue_lag, 3),
    qtr_growth = round((total_revenue - lag(total_revenue, n = 3)) / lag(total_revenue, n=3), 3)
  ) %>%
  ungroup()


# find which countries drive the most growth
country_growth_summary <- country_month_df %>%
  group_by(Country) %>%
  summarise(
    total_revenue = sum(total_revenue),
    avg_mom_growth = round(mean(mom_growth, na.rm = TRUE), 3),
    avg_qtr_growth = round(mean(qtr_growth, na.rm = TRUE), 3)
  ) %>%
  arrange(desc(total_revenue))


# finding contribution to overall growth
global_monthly <- country_month_df %>%
  group_by(year_month) %>%
  summarise(monthly_global_revenue = sum(total_revenue))

total_global_growth <- last(global_monthly$monthly_global_revenue) - first(global_monthly$monthly_global_revenue)

# adding more features
# country_rev_growth_monthly shows units of money (dollars? pounds?)
# country_rev_growth_prct shows prct change in country rev since last month
country_month_df <- country_month_df %>%
  left_join(global_monthly, by = 'year_month') %>%
  mutate(
    country_rev_growth_monthly = total_revenue - lag(total_revenue),
    country_rev_growth_prct = round(country_rev_growth_monthly / lag(total_revenue), 4) * 100,
    global_rev_growth_monthly = round(monthly_global_revenue - lag(monthly_global_revenue), 2)
  )

country_growth_rates <- country_month_df %>%
  group_by(Country) %>%
  summarise(
    Avg_Growth_per_Month = mean(country_rev_growth_monthly, na.rm = TRUE),
    Avg_Rate_Growth_Monthly_prct = round(mean(country_rev_growth_prct, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Avg_Growth_per_Month))



# plot showing avg growth of top and bottom 5 countries (by avg. monthly revenue growth)
growth_prep <- country_growth_rates %>%
  mutate(Country = paste0('#', row_number(), ' ', Country))

top_performers_growth <- growth_prep %>%
  slice_head(n=5) %>%
  mutate(group = 'Top Performer')

bottom_performers_growth <- growth_prep %>%
  slice_tail(n=5) %>%
  mutate(group = 'Bottom Performer')

top_bottom_growth <- bind_rows(top_performers_growth, bottom_performers_growth) %>%
  arrange(desc(Avg_Growth_per_Month))

ggplot(data = top_bottom_growth,
       aes(x = reorder(Country, Avg_Growth_per_Month, decreasing = TRUE),
           y = Avg_Growth_per_Month,
           fill = group)) +
  geom_col() +
  geom_text(
    aes(
      y = ifelse(Avg_Growth_per_Month < 0, 0, Avg_Growth_per_Month + 0.05),
      label = scales::comma(Avg_Growth_per_Month, accuracy = 0.01)
    ),
    vjust = -0.2,
    size = 2.5
  ) +
  labs(title = "Top and Bottom Countries by Average Monthly Revenue Growth",
       x = "Country",
       y = "Average Growth per Month",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# plot showing top and bottom 5 countries based on total revenue
rev_prep <- country_growth_summary %>%
  mutate(Country = paste0('#', row_number(), ' ', Country))

top_performers_rev <- rev_prep %>%
  slice_head(n=5) %>%
  mutate(group = 'Top Performer')

bottom_performers_rev <- rev_prep %>%
  slice_tail(n=5) %>%
  mutate(group = 'Bottom Performer')

top_bottom_rev <- bind_rows(top_performers_rev, bottom_performers_rev) %>%
  arrange(desc(total_revenue))

# top and bottom 5 by revenue generated
ggplot(data = top_bottom_rev,
       aes(x = reorder(Country, total_revenue, decreasing = TRUE),
           y = total_revenue,
           fill = group)) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(total_revenue, accuracy = 0.01)),
    vjust = -0.2,
    size = 2.5
  ) +
  labs(title = "Top and Bottom Countries by Total Revenue Generated",
       x = "Country",
       y = "Total Revenue",
       fill = "Group") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# UK is dominating the chart
# same thing - UK
ggplot(data = top_bottom_rev[-1,],
       aes(x = reorder(Country, total_revenue, decreasing = TRUE),
           y = total_revenue,
           fill = group)) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(total_revenue, accuracy = 0.01)),
    vjust = -0.2,
    size = 2.5
  ) +
  labs(title = "Top and Bottom Countries by Total Revenue (Minus U.K.)",
       x = "Country",
       y = "Total Revenue",
       fill = "Group") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# top 5 countries based on total revenue and revenue growth are the same (places 4 and 5 swap)








############# predicting churn

# calculate a global recency percentile that will serve as a "churn flag"
# calculate days between orders per invoice per customer, exclude probable return exchanges
invoice_summary$InvoiceDate <- ymd_hms(invoice_summary$InvoiceDate)


# compute days between orders per customer
# churn is a retention metric that should apply to REPEAT customers, especially in
# retail where one-time customers are very common.
# remove one-time customers to restrict churn metrics to repeat customers
purchase_intervals <- invoice_summary %>%
  arrange(CustomerID, InvoiceDate) %>%
  group_by(CustomerID) %>%
  filter(returns_flag == FALSE) %>% # remove probable return exchanges
  mutate(
    n_orders = n(),
    prev_invoice_date = lag(InvoiceDate),
    days_btwn_orders = round(as.numeric(difftime(InvoiceDate, prev_invoice_date, units = "days")), 2)
  ) %>%
  filter(n_orders > 1) %>% # only repeat buyers
  ungroup()


# exclude nulls and 0s
purchase_intervals <- purchase_intervals %>%
  filter(!is.na(days_btwn_orders),
         days_btwn_orders > 0)

summary(purchase_intervals$days_btwn_orders)

hist(purchase_intervals$days_btwn_orders)

# define churn threshold as > 95th percentile of days between orders
churn_threshold <- quantile(
  purchase_intervals$days_btwn_orders,
  probs = 0.95,
  na.rm = TRUE
)

# 95th percentile = 206
# if a customer doesn't make a purchase within 206 days, label positive for churn
repeat_customers <- customer_rfm %>%
  filter(n_orders > 1) %>% # only repeat customers
  mutate(
    churn_flag = ifelse(recency_days >= churn_threshold, TRUE, FALSE)
  )

# check to see what proportion of customers are labeled as churned
mean(repeat_customers$churn_flag)
# 28.5% of customers are labeled churn. this is a normal proportion for retail

# another plot for segments, this time with only repeat customers
rfm_repeat_summary <- repeat_customers %>%
  group_by(segment) %>%
  summarise(segment_freq = n())

ggplot(data = rfm_repeat_summary,
       aes(x = reorder(segment, segment_freq),
           y = segment_freq,
           fill = segment)) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(segment_freq)),
    vjust = -0.2,
    size = 3.5
  ) +
  labs(title = "Segmentation Frequencies of Repeat Customers",
       x = "Segment",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none")
# compared to segment frequencies for ALL customers, there are significantly fewer
# instances of "no segment" and "low engagement"
# this is unsurprising as no segment and low engagement are more probable to be one-time customers


# this diagnostic plot shows that n_orders has non-linear negative correlation with recency. customers that 
# have not purchased recently are more likely to skew churn_threshold (red line) upwards, potentially contributing to an
# inaccurate/ineffective churn threshold
# in other words, this provides more reason to exclude one-time customers from calculating the churn threshold
ggplot(customer_rfm,
       aes(x = recency_days,
           y = n_orders)) +
  geom_point(alpha = 0.4) +
  geom_vline(xintercept = churn_threshold,
             color = "red",
             linetype = "dashed") +
  labs(
    title = "Customer Recency vs Purchase Frequency",
    x = "Days Since Last Purchase",
    y = "Number of Orders"
  )



############################# predictive modeling for churn

# predict churn_flag label based on predictors in repeated_customers (excluding recency because it's a near-perfect predictor)

# convert character variables to factors
factor_countries <- as.factor(repeat_customers$country)
levels(factor_countries)
repeat_customers$country <- factor_countries

factor_segment <- as.factor(repeat_customers$segment)
levels(factor_segment)
repeat_customers$segment <- factor_segment

# corrplot only for variables to be included in the model
# exclude dates, customerID, rfm_score (data is already captured in individual RFM components), recency_days (near perfect predictor for churn)
# exclude interpurchase_days_mean bc median will be more robust to outliers
# exclude first and last_order_date because this data is captured in tenure_days
# exclude rfm_score, rfm_total, and r_score due to further multicollinearity and data leakage issues
# exclude segment because of data leakage; ie. anything that uses r_score to calculate should be excluded
# remove repeat_flag because it will be true for all customers in repeat_customers (causes aliasing issues when calling vif())


num_repeat_customers <- repeat_customers %>%
  mutate(churn_flag = ifelse(repeat_customers$churn_flag == TRUE, 1, 0)) %>%
  select(where(is.numeric), -CustomerID, -recency_days, -interpurchase_days_mean, -rfm_score, -r_score, -rfm_total)

cor_matrix <- cor(num_repeat_customers)
corrplot(cor_matrix, method = 'number', number.cex = 0.4, tl.cex = 0.5)

colnames(repeat_customers)

# define initial predictors
predictors <- c('total_revenue',
                'n_orders',
                'avg_order_value',
                'tenure_days',
                'invoices_w_returns',
                'prop_invoices_returned',
                'avg_items_per_invoice',
                'total_unique_skus',
                'country',
                'interpurchase_days_median',
                'months_active',
                'orders_per_month',
                'preferred_month',
                'avg_monthly_revenue',
                'f_score',
                'm_score'
)

churn_data <- repeat_customers %>%
  select(churn_flag, all_of(predictors))

churn_logreg <- glm(churn_flag ~ ., data = churn_data, family = 'binomial')
summary(churn_logreg)

# checking VIF
vif(churn_logreg)

# remove vars based on vif>10 one at a time
# there is one variable with vif>10: n_orders
churn_logreg <- glm(churn_flag ~ . - n_orders, data = churn_data, family = 'binomial')

vif(churn_logreg)
# avg_monthly_revenue has vif = 9.94
# not quite at the threshold but avg_monthly_revenue is closely derived from total_revenue / months_active,
# so it should be removed

churn_logreg <- glm(churn_flag ~ . -n_orders -avg_monthly_revenue, data = churn_data, family = 'binomial')
vif(churn_logreg)

# months active has vif=6.04. likely highly correlated with tenure_days, so we don't need months_active
churn_logreg <- glm(churn_flag ~ . -n_orders -avg_monthly_revenue -months_active, data = churn_data, family = 'binomial')
vif(churn_logreg)
# all vif < 5

formula_ml <- churn_flag ~ . -n_orders -avg_monthly_revenue -months_active




########### training logistic regression model

# need to convert churn_flag to factor for ROC/AUC curves
churn_data$churn_flag <- as.factor(churn_data$churn_flag)
levels(churn_data$churn_flag)

train_index <- createDataPartition(churn_data$churn_flag, p = 0.7, list = FALSE)
train <- churn_data[train_index,]
test <- churn_data[-train_index,]

test$log_prob_churn <- predict(churn_logreg, newdata = test, type = 'response')
test$log_pred_churn <- ifelse(test$log_prob_churn >= 0.5, 'TRUE', 'FALSE') %>%
  factor(levels = c('FALSE', 'TRUE'))

# ROC/AUC curves
roc_churn <- roc(test$churn_flag, test$log_prob_churn)
auc_churn <- roc_churn$auc
print(confusionMatrix(test$log_pred_churn, test$churn_flag, positive = 'TRUE'))
# accuracy: 80.2%
# sensitivity: 52.3%
# specificity: 90.8%



########### training random forest model
rf_model <- randomForest(x = model.matrix(formula_ml, data = train)[, -1],
                         y = train$churn_flag,
                         ntree = 150,
                         mtry = floor(sqrt(length(all.vars(formula_ml)))),
                         nodesize = 1,
                         importance = 50)
varImpPlot(rf_model, main = 'Random Forest Importance')

test_mm <- model.matrix(formula_ml, data = test)[,-1]
test$rf_prob_churn <- predict(rf_model, newdata = test_mm, type = 'prob')[,'TRUE']
test$rf_pred_churn <- ifelse(test$rf_prob_churn > 0.5, 'TRUE', 'FALSE') %>%
  factor(levels = c('FALSE', 'TRUE'))

roc_rf <- roc(test$churn_flag, test$rf_prob_churn)
auc_rf <- roc_rf$auc
print(paste("Random Forest AUC:", auc_rf))
print(confusionMatrix(test$rf_pred_churn, test$churn_flag, positive = 'TRUE'))
# accuracy: 74.0%
# sensitivity: 6.2%
# specificity: 99.7%


### which predictors are most influential in churn predictions?

# for logistic regression model:
logit_coefs <- summary(churn_logreg)$coefficients

logit_imp <- data.frame(
  variable = rownames(logit_coefs),
  logit_coef = logit_coefs[, "Estimate"],
  logit_p_value = logit_coefs[, "Pr(>|z|)"]
) %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(logit_coef),
    logit_abs_coef = abs(logit_coef)
  ) %>%
  arrange(desc(logit_abs_coef))

# using centered/scaled variables to enable comparison of effect across variables
cs_churn_logreg <- train(
  formula_ml,
  data = train,
  method = "glm",
  family = "binomial",
  preProcess = c("center", "scale")
)

cs_logit_coefs <- summary(cs_churn_logreg)$coefficients

cs_logit_imp <- data.frame(
  variable = rownames(cs_logit_coefs),
  logit_coef = cs_logit_coefs[, "Estimate"],
  logit_p_value = cs_logit_coefs[, "Pr(>|z|)"]
) %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(logit_coef),
    logit_abs_coef = abs(logit_coef),
    variable = gsub("`", "", variable)
  ) %>%
  arrange(desc(logit_abs_coef))


# for random forest model:
rf_imp <- importance(rf_model)

rf_imp <- data.frame(
  variable = rownames(rf_imp),
  rf_importance = rf_imp[,1]
) %>%
  arrange(desc(rf_importance))


# combining into one table
importance_table <- cs_logit_imp %>%
  select(variable, logit_coef, odds_ratio, logit_p_value, logit_abs_coef) %>%
  full_join(rf_imp, by = "variable") %>%
  arrange(desc(logit_abs_coef)) %>%
  mutate(logit_rank = dense_rank(desc(logit_abs_coef)),
         rf_rank = dense_rank(desc(rf_importance)))


# there are lots of country variables. the overwhelming proportion of variables that are country dummies is likely diluting importance of
# other, more important variables. to see if we can reasonably exclude them, let's see how many customers there were for each country

cust_per_country <- country_month_df2 %>%
  group_by(Country) %>%
  summarise(
    total_customers = sum(total_customers)) %>%
  arrange(total_customers)


sum(cust_per_country$total_customers) # the UK accounts for about 91% of total customers
# instead of dropping all countries or using top 5, the customers will be grouped using a Boolean, 'fromUK"























######################################## refitting models with boolean country

# define initial predictors
predictors <- c('total_revenue',
                'n_orders',
                'avg_order_value',
                'tenure_days',
                'invoices_w_returns',
                'prop_invoices_returned',
                'avg_items_per_invoice',
                'total_unique_skus',
                'fromUK',
                'interpurchase_days_median',
                'months_active',
                'orders_per_month',
                'preferred_month',
                'avg_monthly_revenue',
                'f_score',
                'm_score'
)

# creating fromUK variable and converting preferred_month to categorical
churn_data <- repeat_customers %>%
  mutate(fromUK = ifelse(country == 'United Kingdom', TRUE, FALSE),
         preferred_month = as.factor(preferred_month)) %>%
  select(churn_flag, all_of(predictors))

churn_logreg <- glm(churn_flag ~ ., data = churn_data, family = 'binomial')
summary(churn_logreg)

# checking VIF
vif(churn_logreg)

# remove vars based on vif>10 one at a time
# there is one variable with vif>10: n_orders
churn_logreg <- glm(churn_flag ~ . - n_orders, data = churn_data, family = 'binomial')

vif(churn_logreg)
# avg monthly revenue has vif = 8.84
# this data is likely captured through total_revenue and months_active, so we can remove
churn_logreg <- glm(churn_flag ~ . - n_orders -avg_monthly_revenue, data = churn_data, family = 'binomial')

vif(churn_logreg)

# months active has vif=5.86. likely highly correlated with tenure_days, so we don't need months_active
churn_logreg <- glm(churn_flag ~ . -n_orders -avg_monthly_revenue -months_active, data = churn_data, family = 'binomial')
vif(churn_logreg)
# all vif < 5

formula_ml <- churn_flag ~ . -n_orders -avg_monthly_revenue -months_active




### re-training logistic regression model with updated variables:
# country has now been reduced to a Boolean: fromUK = TRUE/FALSE
# preferred_month converted to categorical

# need to convert churn_flag to factor for ROC/AUC curves
churn_data$churn_flag <- as.factor(churn_data$churn_flag)
levels(churn_data$churn_flag)

train_index <- createDataPartition(churn_data$churn_flag, p = 0.7, list = FALSE)
train <- churn_data[train_index,]
test <- churn_data[-train_index,]

test$log_prob_churn <- predict(churn_logreg, newdata = test, type = 'response')
test$log_pred_churn <- ifelse(test$log_prob_churn >= 0.5, 'TRUE', 'FALSE') %>%
  factor(levels = c('FALSE', 'TRUE'))

# ROC/AUC curves
roc_churn <- roc(test$churn_flag, test$log_prob_churn)
auc_churn <- roc_churn$auc
print(confusionMatrix(test$log_pred_churn, test$churn_flag, positive = 'TRUE'))
# accuracy: 79.8%
# sensitivity: 53.4%
# spcificity: 89.7%


### re-training random forest model
rf_model <- randomForest(x = model.matrix(formula_ml, data = train)[, -1],
                         y = train$churn_flag,
                         ntree = 150,
                         mtry = floor(sqrt(length(all.vars(formula_ml)))),
                         nodesize = 1,
                         importance = 50)
varImpPlot(rf_model, main = 'Random Forest Importance')

test_mm <- model.matrix(formula_ml, data = test)[,-1]
test$rf_prob_churn <- predict(rf_model, newdata = test_mm, type = 'prob')[,'TRUE']
test$rf_pred_churn <- ifelse(test$rf_prob_churn > 0.5, 'TRUE', 'FALSE') %>%
  factor(levels = c('FALSE', 'TRUE'))

roc_rf <- roc(test$churn_flag, test$rf_prob_churn)
auc_rf <- roc_rf$auc
print(paste("Random Forest AUC:", auc_rf))
print(confusionMatrix(test$rf_pred_churn, test$churn_flag, positive = 'TRUE'))
# accuracy: 78.0%
# sensitivity: 43.4%
# specificity: 91.2%
# this RF model's performance improved markedly from the model trained on the old dataset with categorical countries


############# which predictors are most influential in churn predictions?

# for logistic regression model:
logit_coefs <- summary(churn_logreg)$coefficients

logit_imp <- data.frame(
  variable = rownames(logit_coefs),
  logit_coef = logit_coefs[, "Estimate"],
  logit_p_value = logit_coefs[, "Pr(>|z|)"]
) %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(logit_coef),
    logit_abs_coef = abs(logit_coef)
  ) %>%
  arrange(desc(logit_abs_coef))

# using centered/scaled variables to enable comparison of effect across variables
cs_churn_logreg <- train(
  formula_ml,
  data = train,
  method = "glm",
  family = "binomial",
  preProcess = c("center", "scale")
)

cs_logit_coefs <- summary(cs_churn_logreg)$coefficients

cs_logit_imp <- data.frame(
  variable = rownames(cs_logit_coefs),
  logit_coef = cs_logit_coefs[, "Estimate"],
  logit_p_value = cs_logit_coefs[, "Pr(>|z|)"]
) %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    odds_ratio = exp(logit_coef),
    logit_abs_coef = abs(logit_coef),
    variable = gsub("`", "", variable)
  ) %>%
  arrange(desc(logit_abs_coef))


# for random forest model:
rf_imp <- importance(rf_model)

rf_imp <- data.frame(
  variable = rownames(rf_imp),
  rf_importance = rf_imp[,1]
) %>%
  arrange(desc(rf_importance))


# combining into one table
importance_table <- cs_logit_imp %>%
  select(variable, logit_coef, odds_ratio, logit_p_value, logit_abs_coef) %>%
  full_join(rf_imp, by = "variable") %>%
  arrange(desc(logit_abs_coef)) %>%
  mutate(logit_rank = dense_rank(desc(logit_abs_coef)),
         rf_rank = dense_rank(desc(rf_importance)))

# tenure_days is the single most important variable in predicting churn in both the rf and logistic regression models
# after that, the models vary in rankings, but preferred months make up the majority of the top ten most influential predictors for the logistic regression model
# for the rf model, the second and third most influential predictors were total_revenue and interpurchase_days_median, a measure of frequency





















