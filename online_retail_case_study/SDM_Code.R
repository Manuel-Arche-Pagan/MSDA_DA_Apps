# initializing data + environment
packages <- c('car', 'tidyverse', 'data.table', 'caret', 'pROC', 'rpart', 'rpart.plot', 'randomForest', 'broom', 'readxl', 'themis', 'recipes', 'MLmetrics')
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




################## Fraud and Operations Detection  ################## 

############## Anomaly/Fraud/Return Feature Engineering ############## 

# Adding Sales Total. This also will calculate negative quantities that will be
# flagged later.

df_fraud <- df_all |> 
  mutate(
    InvoiceDate = as.POSIXct(InvoiceDate),
    TotalValue = Quantity * Price
  )

inconsistent_codes <- df_fraud |> 
  filter(!is.na(Description)) |> 
  group_by(StockCode) |> 
  summarize(desc_count = n_distinct(Description)) |> 
  filter(desc_count > 1)

df_fraud <- df_fraud |> 
  mutate(is_inconsistent_stock = StockCode %in% inconsistent_codes$StockCode)

# This logic flags system entries with Invoices and descriptions that may be
# credit notes, administrative corrections/cancellations, verified returns, 
# and other types of anomalies.

# Weird or Admin codes for descriptions of anomalous entries
non_product_codes <- c("DOT", "POST", "D", "M", "BANK CHARGES", "AMAZONFEE")

# Define the types of transactions
df_fraud <- df_fraud |> 
  mutate(
    
    # A 'Return Candidate' is any reversal (Negative Qty OR Negative/Suspicious Price OR "C" Invoice)
    is_return_candidate = Quantity < 0 | str_starts(Invoice, "C"),
    
    # A 'Pure Purchase' is the original sale we want to match against
    is_pure_purchase = !is_return_candidate & !is.na(`Customer ID`) & Quantity > 0 & Price > 0 & !(str_starts(Invoice, "A")),
    
    # Debt adjustment entries that account for negative prices (Although, some are positive).  
    is_debt_adjustment = Quantity > 0 & (Price < 0 | str_starts(Invoice, "A")),
    
    # Identify Small Promotions (Price is 0 and Quantity is between 1 and 9)
    is_promotional = Price == 0 & Quantity > 0 & Quantity < 10,
    
    # Identify Large-Scale Inventory Adjustments (Price is 0 but Quantity is high)
    is_inventory_adjustment = Price == 0 & Quantity >= 10
  )

# Build the Audit Trail (Matching)
df_returns <- df_fraud |>  filter(is_return_candidate == TRUE)
df_purchases <- df_fraud |>  filter(is_pure_purchase == TRUE)

verified_matches <- df_returns |> 
  inner_join(
    df_purchases |>  select(Invoice, `Customer ID`, StockCode, Price, Country, Quantity, InvoiceDate),
    by = c("Customer ID", "StockCode", "Price", "Country"),
    suffix = c("_ret", "_purch")
  ) |> 
  filter(InvoiceDate_purch < InvoiceDate_ret,
         abs(Quantity_ret) <= Quantity_purch)

# Final Flagging and Categorization
df_fraud <- df_fraud |> 
  mutate(
    # Core Boolean Flags for easy filtering later
    is_verified_return = is_return_candidate & Invoice %in% verified_matches$Invoice_ret,
    is_unverified_negative = is_return_candidate & !Invoice %in% verified_matches$Invoice_ret,
    
    # The Master Classification
    Anomaly_Classification = case_when(
      # Healthy Sales (Must be checked first)
      is_pure_purchase ~ "Standard Sale",
      
      # Verified Returns (Matched against a previous sale)
      is_verified_return ~ "Verified Customer Return",
      
      # Non-Product Code Logic (Postage, Bank Fees, etc.)
      StockCode %in% non_product_codes ~ "Admin/Fee Adjustment",
      
      # Financial & Debt Adjustments (Using your 'A' Invoice/Negative Price logic)
      is_debt_adjustment ~ "Debt/Financial Adjustment",
      
      # Marketing/Inventory Price-Zero Logic
      is_promotional ~ "Small Promo/Gift",
      
      is_inventory_adjustment ~ "Bulk Inventory Adjustment",
      
      # Data Integrity Issues
      # Label Unverified Negatives Quantities
      is_unverified_negative ~ "Unverified Negative",
      
      # Label Entries that may be positive but are not tied to any already
      # classified transactions
      is.na(`Customer ID`) ~ "Missing Customer ID",
      
      # Catch-all for anything that doesn't fit above
      TRUE ~ "Other Adjustment"
    )
  )



# Calculate the threshold based on absolute value (to capture huge returns too)
threshold <- quantile(abs(df_fraud$TotalValue), 0.99, na.rm = TRUE)

df_fraud <- df_fraud |> 
  mutate(is_high_value = abs(TotalValue) >= threshold)




############## End of Anomaly/Fraud/Return Feature Engineering ############## 


############## Anomaly/Fraud/Return EDA ##################################### 

# Are there anomalies in transaction data (e.g., negative quantities, high-value orders)?


#### Top Level Anomaly Detection Analysis

# Create a summary table of flags
flag_counts <- c(
  Returns = sum(df_fraud$Anomaly_Classification == "Verified Customer Return", na.rm=TRUE),
  Promo = sum(df_fraud$Anomaly_Classification == "Small Promo/Gift", na.rm=TRUE),
  Debt_Adj = sum(df_fraud$Anomaly_Classification == "Debt/Financial Adjustment", na.rm=TRUE),
  Inventory_Adj = sum(df_fraud$Anomaly_Classification == "Bulk Inventory Adjustment", na.rm=TRUE),
  Admin_Fee_Adj = sum(df_fraud$Anomaly_Classification == "Admin/Fee Adjustment" , na.rm=TRUE),
  Miss_CustomerID = sum(df_fraud$Anomaly_Classification == "Missing Customer ID" , na.rm=TRUE),
  Unverified_Neg_Trans = sum(df_fraud$Anomaly_Classification == "Unverified Negative" , na.rm=TRUE),
  High_Value_Trans = sum(df_fraud$is_high_value, na.rm=TRUE)
) |> sort(decreasing = TRUE)



par(mar=c(12, 5, 4, 2))  

bp <- barplot(flag_counts, 
              main="Top Level Anomaly Detection Analysis",
              col=c("firebrick", "orange", "steelblue","green", "blue", "purple", "black", "seagreen3"),
              las = 3,           # Rotate category labels 90 degrees
              ylab="Number of Transactions",
              ylim=c(0, max(flag_counts)*1.2))

text(x = bp, 
     y = flag_counts, 
     label = format(flag_counts, big.mark = ","),
     pos = 3, 
     cex = 0.8, 
     font = 2)


#### Ratio of High Value Transactions that are Sales vs Anomalies(Negatives)

# Create a summary of High-Value Transactions
# We filter for only the high_value flag and then split by the purchase/return flags
hv_summary <- df_fraud |> 
  filter(is_high_value == TRUE) |> 
  mutate(HV_Type = ifelse(is_pure_purchase, "High-Value Sales", "High-Value Anomalies")) |> 
  count(HV_Type)

# Prepare labels with commas and percentages
hv_summary$percentage <- round(hv_summary$n / sum(hv_summary$n) * 100, 1)

# Format the labels: "Category \n Count (Percentage%)"
hv_labels <- paste0(hv_summary$HV_Type, "\n", 
                    format(hv_summary$n, big.mark=","), 
                    " (", hv_summary$percentage, "%)")


colors <- c("firebrick2", "seagreen3")

par(mar=c(2, 2, 4, 2)) 

pie(hv_summary$n, 
    labels = hv_labels, 
    col = colors, 
    main = "High-Value Transaction Composition",
    init.angle = 90, 
    cex = 0.9,
    border = "white")


#### #### Breakdown of High Value Transactions that are Sales vs Anomalies(Negatives)

# Summarize High-Value Transactions by Dollar Amount
hv_financials <- df_fraud |> 
  filter(is_high_value == TRUE) |> 
  group_by(Anomaly_Classification) |> 
  summarize(Total_Dollar = sum(abs(TotalValue), na.rm = TRUE)) |> 
  arrange(Total_Dollar) # Arranging for the horizontal plot (smallest to largest)

# Set margins to accommodate long category names

par(mar=c(5, 15, 4, 2)) 

# Create the Horizontal Bar Plot
bp <- barplot(hv_financials$Total_Dollar, 
              names.arg = hv_financials$Anomaly_Classification, 
              horiz = TRUE, 
              las = 1, 
              col = "firebrick3", # High-risk color
              main = "Exposure of High-Value Transactions",
              xlab = "Total Absolute Value ($)",
              xlim = c(0, max(hv_financials$Total_Dollar) * 1.3)) # Extra room for labels

# Add the Dollar Labels with Comma Formatting
text(x = hv_financials$Total_Dollar, 
     y = bp, 
     label = paste0("$", format(round(hv_financials$Total_Dollar, 0), big.mark=",")), 
     pos = 4, 
     cex = 0.8, 
     font = 2)


#### Standard Sales Vs All Anomalies

# Create a simplified bucket for the visual
df_summary <- df_fraud |> 
  mutate(Audit_Bucket = ifelse(Anomaly_Classification == "Standard Sale", 
                               "Standard Sales", 
                               "Total Anomalies")) |> 
  count(Audit_Bucket)

# Calculate percentages for the labels
df_summary$percentage <- round(df_summary$n / sum(df_summary$n) * 100, 1)
label_text <- paste0(df_summary$Audit_Bucket, "\n", 
                     format(df_summary$n, big.mark=","), " (", 
                     df_summary$percentage, "%)")

# Create the Pie Chart
# Standard Sales usually dominate (~97-98%), so we use contrasting colors
colors <- c("firebrick2", "seagreen3")

par(mar=c(2, 2, 2, 2)) 

pie(df_summary$n, 
    labels = label_text, 
    col = colors, 
    main = "Operational Integrity: Sales vs. All Anomalies",
    init.angle = 90, 
    cex = 0.9,
    border = "white")


#### Frequencies of the Returns/Cancellations/Anomalies 

# Create a frequency table of the categories, excluding "Standard Sale"
# This allows us to see the distribution of the actual anomalies
anomaly_data <- df_fraud |> 
  filter(Anomaly_Classification != "Standard Sale") |> 
  pull(Anomaly_Classification) |> 
  table()

# Sort the data so the tallest bars come first
anomaly_data <- sort(anomaly_data, decreasing = TRUE)


par(mar=c(12, 5, 4, 2)) 

# Create the Bar Plot
bp <- barplot(anomaly_data,
              main = "Audit: Distribution of Cancellations & Anomalies",
              col = c("firebrick", "orange", "steelblue","green", "blue", "purple","seagreen3"),
              las = 3,           # Rotate category labels 90 degrees
              cex.names = 0.8,   # Shrink label text slightly to fit
              ylab = "Number of Transactions",
              ylim = c(0, max(anomaly_data) * 1.1)) 

# Add exact counts on top of the bars
text(x = bp, 
     y = anomaly_data, 
     label = anomaly_data, 
     pos = 3, 
     cex = 0.8, 
     font = 2)

#### Impact in Dollar amounts of the Returns/Cancellations/Anomalies 


# Calculate the Financial Impact (Sum of Total Value)
# We filter out "Standard Sale" to focus on the costs of cancellations
financial_summary <- df_fraud |> 
  #filter(Anomaly_Classification != "Standard Sale") |> 
  group_by(Anomaly_Classification) |> 
  summarize(Total_Loss = sum(TotalValue, na.rm = TRUE)) |> 
  mutate(Total_Loss = abs(Total_Loss)) |>  # Convert negative impact to positive bars
  arrange(desc(Total_Loss))


par(mar=c(12, 8, 4, 2)) 

# Create the Bar Plot
bp <- barplot(financial_summary$Total_Loss,
              names.arg = financial_summary$Anomaly_Classification,
              main = "Financial Impact: Value of Returns & Anomalies",
              col = c("forestgreen","firebrick", "orange", "steelblue","grey", "blue", "purple","seagreen3"),
              las = 2,              # Vertical labels
              cex.names = 0.8,      # Label size
              ylim = c(0, max(financial_summary$Total_Loss) * 1.2))

# Add Dollar Labels on top of each bar
text(x = bp, 
     y = financial_summary$Total_Loss, 
     label = paste0("$", format(round(financial_summary$Total_Loss, 0), big.mark=",")), 
     pos = 3, 
     cex = 0.8, 
     font = 2)



#### Return/Cancellation Rates of Countries, Top 10 Visual


# Calculate Return Rate by Country
region_analysis <- df_fraud |> 
  group_by(Country) |> 
  summarize(
    total_transactions = n(),
    verified_returns = sum(is_verified_return),
    # Business definition: Return rate = # of returns / total transactions
    return_rate = (verified_returns / total_transactions)
  ) |> 
  arrange(desc(return_rate))

# Get top 10 countries, but exclude those with fewer than 500 transactions

top_10_regions <- region_analysis |> 
  filter(total_transactions > 500) |> 
  head(10)

top_10_regions <- top_10_regions[order(top_10_regions$return_rate), ]

# Top 10 High-Risk Regions
top_10_regions


par(mar=c(5, 10, 4, 2))


bp <- barplot(top_10_regions$return_rate, 
              names.arg = top_10_regions$Country, 
              horiz = TRUE, 
              las = 1, 
              col = "seagreen3", 
              main = "Top 10 High-Risk Regions: Verified Return Rate",
              xlab = "Return Rate (Ratio of Total Transactions)",
              xlim = c(0, max(top_10_regions$return_rate) * 1.2)) # Add space for labels

# Add the percentage labels at the end of each bar
# We multiply by 100 and round for a clean display
text(x = top_10_regions$return_rate, 
     y = bp, 
     label = paste0(round(top_10_regions$return_rate * 100, 2), "%"), 
     pos = 4, 
     cex = 0.8, 
     font = 2)


#### Return/Cancellation Rates of Products, Top 10 Visual

# Calculate Return Rate by Product (Description)
product_impact <- df_fraud |> 
  group_by(Description) |> 
  summarize(
    total_transactions = n(),
    verified_returns = sum(is_verified_return, na.rm = TRUE),
    # Return rate calculation
    return_rate = (verified_returns / total_transactions)
  ) |> 
  # Sort by the count of returns to find the most frequent products that are returned
  arrange(desc(verified_returns))

# Select top 10 most returned items and re-order for plotting
top_10_impact <- product_impact |> 
  head(10) |> 
  arrange(verified_returns) # Order ascending for the barplot display


par(mar=c(5, 15, 4, 8)) 

bp <- barplot(top_10_impact$verified_returns, 
              names.arg = top_10_impact$Description, 
              horiz = TRUE, 
              las = 1, 
              col = "firebrick2", 
              main = "Top 10 Products by Volume of Verified Returns",
              xlab = "Number of Returns (Count)",
              cex.names = 0.7,
              xlim = c(0, max(top_10_impact$verified_returns) * 1.2))

# Add the Return Rate (%) as the label on the bars
# This shows the "Ratio" context for the "Volume" problem
text(x = top_10_impact$verified_returns, 
     y = bp, 
     label = paste0(format(top_10_impact$verified_returns, big.mark=","), 
                    " (", round(top_10_impact$return_rate * 100, 1), "%)"), 
     pos = 4, 
     cex = 0.8, 
     font = 2)


############## End Of Anomaly/Fraud/Return EDA ##############################

############## Anomaly/Fraud/Return Model ##################################

############## READ ME BEFORE BUILDING/RUNNING MODEL #######################

# this model takes a while to run, the df and model are separate from the rest
# of the project.  If you have the time, then  go ahead, but beware of the run 
# time. Can be very taxing on the system.  I commented the results from the final
# results table to save time.
# THE CODE WILL FINISH AND IT WORKS THOUGH.
# Well, I guess that might be hardware dependent.

############################################################################

# Data Preparation

df_fraud_safe <- df_fraud |> 
  mutate(Anomaly_Classification = make.names(Anomaly_Classification))

model_df <- df_fraud_safe |>
  mutate(
    target = as.factor(Anomaly_Classification),
    is_high_value         = as.factor(is_high_value),
    is_return_candidate   = as.factor(is_return_candidate),
    # Extract base signal from raw fields without creating classification flags
    invoice_prefix        = substr(Invoice, 1, 1),          # "C", "A", or normal
    is_admin_stockcode    = as.factor(StockCode %in%
                                        c("DOT","POST","D","M","BANK CHARGES","AMAZONFEE")),
    TotalValue            = Quantity * Price
  ) |>
  select(target, Quantity, Price, Country, TotalValue,
         is_high_value, is_return_candidate,
         invoice_prefix, is_admin_stockcode) |>
  na.omit()

set.seed(42)

# Stratified Train/Test Split 
trainIndex <- createDataPartition(model_df$target, p = 0.8, list = FALSE)
train_data <- model_df[trainIndex, ]
test_data  <- model_df[-trainIndex, ]

# Hybrid Resampling Recipe
# We oversample the rare anomalies and downsample the standard sales

rec <- recipe(target ~ ., data = train_data) |> 
  step_novel(all_nominal_predictors()) |>       # Handles new countries in test set
  step_dummy(all_nominal_predictors(), -target) |>  
  step_upsample(target, over_ratio = 0.5) |>       # Boost minority classes
  step_downsample(target, under_ratio = 1)      # Reduce majority class

train_balanced <- prep(rec) |>  bake(new_data = NULL)

# Compute Class Weights
# Even after balancing, weights help the model "pay attention" to critical errors

class_counts <- table(train_balanced$target)
class_weights <- max(class_counts) / class_counts


library(doParallel) # To help speed up training
library(ranger)


ctrl <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = "final"
)

# Tuning mtry: number of variables available at each tree split
tune_grid_ranger <- expand.grid(
  mtry          = c(2, 3, 4),
  splitrule     = "gini",
  min.node.size = 5
)

### Helps speed up run time ####

# Kill any existing clusters
try(stopCluster(cl), silent = TRUE)

# Reset the parallel backend to sequential
registerDoSEQ()

# Force garbage collection to free memory held by dead workers
gc()

#################################

rf_tuned <- train(
  
  target ~ .,
  data = train_balanced,
  method = "ranger",
  trControl = ctrl,
  metric = "Mean_F1",
  tuneGrid = tune_grid_ranger,
  ntree = 200,
  importance = "impurity",
  classwt = class_weights,
  num.threads = parallel::detectCores() - 1
)


# Note: We must bake the test data if we used dummy variables in recipe
test_baked <- bake(prep(rec), new_data = test_data)
predictions <- predict(rf_tuned, newdata = test_baked)
probs <- predict(rf_tuned, newdata = test_baked, type = "prob")

# Final Results
cm <- confusionMatrix(predictions, test_baked$target)
print(cm$table)


auc_scores <- sapply(levels(test_baked$target), function(cls) {
  auc(as.numeric(test_baked$target == cls), probs[[cls]], quiet = TRUE)
})

print(
  as.data.frame(cm$byClass[, c("Precision", "Recall", "F1")]) |>
    mutate(AUC       = round(auc_scores, 4),
           Precision = round(Precision, 4),
           Recall    = round(Recall, 4),
           F1        = round(F1, 4))
)


################  FINAL RANDOM FOREST RESULTS ##################

#                                 Precision Recall     F1    AUC
# Class: Admin.Fee.Adjustment         0.6864 0.9980 0.8133 0.9998
# Class: Bulk.Inventory.Adjustment    1.0000 1.0000 1.0000 1.0000
# Class: Debt.Financial.Adjustment    1.0000 1.0000 1.0000 1.0000
# Class: Missing.Customer.ID          0.4286 0.8358 0.5666 0.8467
# Class: Small.Promo.Gift             0.9969 1.0000 0.9985 1.0000
# Class: Standard.Sale                0.9339 0.6745 0.7832 0.8611
# Class: Unverified.Negative          1.0000 0.6458 0.7848 0.9978
# Class: Verified.Customer.Return     0.8965 0.9878 0.9400 0.9993
################################################################

############## End Anomaly/Fraud/Return Model ##############################


################## End of Fraud and Operations Detection  ###################

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

library(corrplot)

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

set.seed(123)

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

set.seed(123)

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
# accuracy: 82.1%
# sensitivity: 58.0%
# specificity: 91.3%


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
# accuracy: 80.0%
# sensitivity: 44.7%
# specificity: 93.4%
# this RF model's performance improved markedly from the model trained on the old dataset with categorical countries
# sensitivity is still an issue



############### attempt to minimize type II error by balancing splits
churn_data$churn_flag <- as.factor(churn_data$churn_flag)
levels(churn_data$churn_flag)

set.seed(123)
churn_data_t <- filter(churn_data, churn_flag == TRUE)
churn_data_f <- filter(churn_data, churn_flag == FALSE)
churn_data_f_random <- sample_n(churn_data_f, dim(churn_data_t)[1])
churn_data_bal <- rbind(churn_data_t, churn_data_f_random)

nrow(filter(churn_data_bal, churn_flag == FALSE))


train_index <- createDataPartition(churn_data_bal$churn_flag, p = 0.7, list = FALSE)
train <- churn_data_bal[train_index,]
test <- churn_data_bal[-train_index,]

churn_logreg <- glm(churn_flag ~ . -n_orders -avg_monthly_revenue -months_active, data = churn_data_bal, family = 'binomial')

test$log_prob_churn <- predict(churn_logreg, newdata = test, type = 'response')
test$log_pred_churn <- ifelse(test$log_prob_churn >= 0.5, 'TRUE', 'FALSE') %>%
  factor(levels = c('FALSE', 'TRUE'))

# ROC/AUC curves
roc_churn <- roc(test$churn_flag, test$log_prob_churn)
auc_churn <- roc_churn$auc
print(confusionMatrix(test$log_pred_churn, test$churn_flag, positive = 'TRUE'))
# accuracy: 79.5%
# sensitivity: 84.6%
# specificity: 74.5%

### re-training random forest model with balanced data
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
# accuracy: 78.9%
# sensitivity: 84.0%
# specificity: 73.7%

# both models showed substantial improvement in sensitivity score when trained on balanced data
# since we aim to predict churn = TRUE, type II errors are more critical than type I errors
# increasing sensitivity at the cost of specificity is absolutely preferred


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









############################  Promotions Model  ############################ 

### Can we predict which customers are most likely to respond to promotions?

# Promotional Response Likelihood for Customers Model :
# Target: Predict High / Medium / Low promo response tier


library(future)
library(doFuture)




model_promo <- customer_rfm %>%
  mutate(
    promo_tier = case_when(
      segment %in% c("Champion", "Loyal Low Spender")       ~ "High",
      segment %in% c("At-risk High Value", "New Customer")  ~ "Medium",
      TRUE                                                   ~ "Low"
    ),
    promo_tier = factor(promo_tier, levels = c("High", "Medium", "Low"))
  ) %>%
  # # Columns can potential leak too much information to the model during training
  # Exclude: segment (target source), RFM scores/totals (used to build segment)
  # Also exclude date columns — not useful as raw predictors
  select(-r_score, -f_score, -m_score, -rfm_score, -rfm_total,
         -first_order_date, -last_order_date, -segment) %>%
  na.omit()   

# Save surviving IDs before dropping CustomerID
surviving_ids <- model_promo$CustomerID

# Now drop CustomerID for modeling
model_promo <- model_promo %>% select(-CustomerID)

# Verify sizes match
cat("Surviving customers:", length(surviving_ids), "\n")  # should be 4481
cat("model_promo rows:", nrow(model_promo), "\n")          # should be 4481

# Check tier distribution
cat("Promo Tier Distribution:\n")
print(prop.table(table(model_promo$promo_tier)))


set.seed(42)
trainIndex <- createDataPartition(model_promo$promo_tier, p = 0.8, list = FALSE)
train_data <- model_promo[trainIndex, ]
test_data  <- model_promo[-trainIndex, ]


# Encode country, handle imbalance across 3 tiers
rec <- recipe(promo_tier ~ ., data = train_data) |> 
  step_novel(all_nominal_predictors()) |>         # handle unseen countries in test
  step_other(country, threshold = 0.02) |>        # bin rare countries into "other"
  step_dummy(all_nominal_predictors()) |>         # encode country
  step_zv(all_predictors()) |>                    # remove zero-variance columns
  step_normalize(all_numeric_predictors()) |>      # scale numerics for stability
  step_smote(promo_tier, over_ratio = 0.8) |>     # oversample minority tiers
  step_downsample(promo_tier, under_ratio = 1)    # balance majority tier down

train_balanced <- prep(rec) |>  bake(new_data = NULL)

cat("\nBalanced Tier Distribution:\n")
print(prop.table(table(train_balanced$promo_tier)))


ctrl <- trainControl(
  method          = "cv",
  number          = 3,                     # 3-fold for speed
  classProbs      = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = "final"
)

tune_grid <- expand.grid(
  mtry          = c(3, 5, 7),
  splitrule     = "gini",
  min.node.size = 5
)

# Again, this is to help speed things up.  Not sure whats happening at the
# hardware level. Be careful.

plan(multisession, workers = min(3, parallel::detectCores() - 1))
registerDoFuture()

set.seed(42)

rf_promo <- train(
  promo_tier ~ .,
  data      = train_balanced,
  method    = "ranger",
  trControl = ctrl,
  metric    = "Mean_F1",
  tuneGrid  = tune_grid,
  num.trees = 200,
  importance = "impurity",
  verbose   = FALSE
)

plan(sequential)  # reset parallel backend


test_baked  <- bake(prep(rec), new_data = test_data)
predictions <- predict(rf_promo, newdata = test_baked)
probs       <- predict(rf_promo, newdata = test_baked, type = "prob")


cm <- confusionMatrix(predictions, test_baked$promo_tier)
cat("\nConfusion Matrix:\n")
print(cm$table)

# Per-Class Metrics with AUC

auc_scores <- sapply(levels(test_baked$promo_tier), function(cls) {
  auc(as.numeric(test_baked$promo_tier == cls), probs[[cls]], quiet = TRUE)
})
cat("\nPer-Class Metrics:\n")
print(
  as.data.frame(cm$byClass[, c("Precision", "Recall", "F1")]) |>
    mutate(AUC = round(auc_scores, 4),
           Precision = round(Precision, 4),
           Recall    = round(Recall, 4),
           F1        = round(F1, 4))
)


################  PROMO RANDOM FOREST RESULTS ##################

#               Precision Recall     F1    AUC
# Class: High      0.9848 0.9811 0.9829 0.9997
# Class: Medium    1.0000 0.8919 0.9429 0.9995
# Class: Low       0.970 0.9928 0.9849 0.9991
################################################################



# Variable Importance
# The model honed in on the most important factors used in the rfm logic
# from the earlier customer segmentation classification. This makes sense.
cat("\nTop 10 Most Important Features:\n")
imp <- varImp(rf_promo)
print(imp, top = 10)

################  Top 10 Most Important Features ##################

# only 10 most important variables shown (out of 16)
# 
#                     Overall
# recency_days        100.000
# n_orders             55.275
# total_revenue        42.304
# tenure_days          25.186
# months_active        19.101
# avg_order_value       4.246
# avg_monthly_revenue   3.294
# preferred_month       3.136
# orders_per_month      2.885
# total_unique_skus     2.687
################################################################

# SCORE ALL CUSTOMERS
# Apply model to full dataset to rank every customer
full_baked <- bake(prep(rec), new_data = model_promo |> select(-promo_tier))

# Add CustomerID back for reference
scored_customers <- customer_rfm |> 
  filter(CustomerID %in% surviving_ids) |>
  select(CustomerID, segment) |> 
  bind_cols(
    predict(rf_promo, newdata = full_baked, type = "prob")
  ) |> 
  mutate(
    predicted_tier = predict(rf_promo, newdata = full_baked),
    confidence     = pmax(High, Medium, Low)
  ) |> 
  arrange(desc(High), desc(confidence))

cat("\nTop 20 Customers Most Likely to Respond to Promotions:\n")
print(head(scored_customers, 20))

################################ TAKEAWAY #################################### 

# Built the RFM scores ourselves from raw transaction data, 
# then used those scores to create meaningful business segments, 
# and now the model is using the raw behavioral features to predict 
# those segments for new customers.

# Of course recency_days, n_orders, and total_revenue dominate. 
# We deliberately designed our segments around those concepts. 
# The model confirming that is a sign of internal consistency.

# Now:
#
# 1. When a new customer enters the dataset, we can predict their promo tier 
# before they have enough history for full RFM scoring
#
# 2. We can score customers with incomplete transaction history
#
# 3. Assign a tier instantly from basic behavioral signals
# 
# 4. No need to manually recalculate RFM for every new entry


##############################################################################

#########################  End of Promotions Model  ########################## 




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

set.seed(123)

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
# accuracy: 82.1%
# sensitivity: 58.0%
# specificity: 91.3%


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
# accuracy: 80.0%
# sensitivity: 44.7%
# specificity: 93.4%
# this RF model's performance improved markedly from the model trained on the old dataset with categorical countries



############### attempt to minimize type II error by balancing splits
churn_data$churn_flag <- as.factor(churn_data$churn_flag)
levels(churn_data$churn_flag)

set.seed(123)
churn_data_t <- filter(churn_data, churn_flag == TRUE)
churn_data_f <- filter(churn_data, churn_flag == FALSE)
churn_data_f_random <- sample_n(churn_data_f, dim(churn_data_t)[1])
churn_data_bal <- rbind(churn_data_t, churn_data_f_random)

nrow(filter(churn_data_bal, churn_flag == FALSE))


train_index <- createDataPartition(churn_data_bal$churn_flag, p = 0.7, list = FALSE)
train <- churn_data_bal[train_index,]
test <- churn_data_bal[-train_index,]

churn_logreg <- glm(churn_flag ~ . -n_orders -avg_monthly_revenue -months_active, data = churn_data_bal, family = 'binomial')

test$log_prob_churn <- predict(churn_logreg, newdata = test, type = 'response')
test$log_pred_churn <- ifelse(test$log_prob_churn >= 0.5, 'TRUE', 'FALSE') %>%
  factor(levels = c('FALSE', 'TRUE'))

# ROC/AUC curves
roc_churn <- roc(test$churn_flag, test$log_prob_churn)
auc_churn <- roc_churn$auc
print(confusionMatrix(test$log_pred_churn, test$churn_flag, positive = 'TRUE'))
# accuracy: 79.5%
# sensitivity: 84.6%
# specificity: 74.5%

### re-training random forest model with balanced data
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
# accuracy: 78.9%
# sensitivity: 84.0%
# specificity: 73.7%

# both models showed substantial improvement in sensitivity score when trained on balanced data
# since we aim to predict churn = TRUE, type II errors are more critical than type I errors
# increasing sensitivity at the cost of specificity is absolutely preferred


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












