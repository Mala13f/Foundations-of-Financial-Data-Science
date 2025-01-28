# --------------------- FINAL PROJECT R-CODE -----------------------
#                       
#                        EUR/GDP Analysis 

# Group: Lucia de Alarcon, Federica Malamisura, Vasil Naumov and Andrian Gutium
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (0) SET WORKING DIRECTORY AND LOAD PACKAGES
# -----------------------------------------------------------------------------
setwd("~/Desktop/data")
print(getwd())

# Load the necessary packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(stringr)
library(reshape2)  
library(zoo)
library(glmnet)
library(readxl)
library(conflicted)
library(caret)
library(Metrics)
library(rpart)
library(rpart.plot)
library(randomForest)
library(boot)
library(GGally)
library(cluster)
library(corrplot)

# Resolve conflicts by preferring dplyr's filter over stats' filter
conflicts_prefer(dplyr::filter)

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (1) PREPARE DATA 
# -----------------------------------------------------------------------------

# Load all the data 
eur_usd <- read_excel("eurusd.xlsx")
gdp_eur <- read_excel("gdpeur.xlsx")
eur_interest <- read_excel("interesteur.xlsx")
eur_inflation <- read_excel("inflationeur.xlsx")
gdp_usd <- read_excel("gdpusa.xlsx")
usd_interest <- read_excel("interestusd.xlsx")
usd_inflation <- read.csv("T10YIEM.csv")

# Assign proper column names for USD Inflation
colnames(usd_inflation) <- c("Date", "Inflation_Rate")

# Data Cleaning Function to avoid repetition
clean_data <- function(df, remove_rows, select_cols, new_col_names, start_date, by) {
  # Remove specified header rows and select desired columns
  df <- df[-c(1:remove_rows), select_cols]
  
  # Rename columns
  colnames(df) <- new_col_names
  
  # Calculate the number of rows after removal
  length_out <- nrow(df)
  
  # Generate dates based on the specified start_date and frequency
  dates <- seq(
    from = as.Date(start_date, format = "%Y-%m-%d"), 
    by = by, 
    length.out = length_out
  )
  
  # Assign the dates to the Date column
  df$Date <- dates
  
  # Verify that the length of dates matches the number of rows
  if(length(dates) != nrow(df)) {
    stop(paste("Length of dates (", length(dates), 
               ") does not match number of rows in dataframe (", 
               nrow(df), ").", sep = ""))
  }
  
  return(as.data.frame(df))
}

# Clean EUR/USD data
eur_usd <- clean_data(
  df = eur_usd, 
  remove_rows = 7, 
  select_cols = 1:2, 
  new_col_names = c("Date", "EUR_USD"), 
  start_date = "2023-12-28", 
  by = "-1 day"
)

# Clean EUR Inflation data
eur_inflation <- clean_data(
  df = eur_inflation, 
  remove_rows = 5, 
  select_cols = 1:2, 
  new_col_names = c("Date", "Inflation_Rate"), 
  start_date = "2023-12-01", 
  by = "-1 month"
)
rownames(eur_inflation) <- eur_inflation$Date

# Clean GDP EUR data (Quarterly)
gdp_eur <- clean_data(
  df = gdp_eur, 
  remove_rows = 5, 
  select_cols = 1:2, 
  new_col_names = c("Date", "GDP_EUR"), 
  start_date = "2023-12-31", 
  by = "-3 month"
)
rownames(gdp_eur) <- gdp_eur$Date

# Clean EUR Interest data
eur_interest <- clean_data(
  df = eur_interest, 
  remove_rows = 5, 
  select_cols = 1:2, 
  new_col_names = c("Date", "INTEREST_EUR"), 
  start_date = "2023-12-29", 
  by = "-1 day"
)

# Clean USD Inflation data
# Remove specified header rows (first 5 rows)
usd_inflation <- usd_inflation[-c(1:5), c(1,2)]
colnames(usd_inflation) <- c("Date", "Inflation_Rate")  # Ensure correct naming

# Convert Date column to Date type
usd_inflation$Date <- as.Date(usd_inflation$Date, format = "%Y-%m-%d")

# Handle any potential NA values in Date
usd_inflation <- usd_inflation %>% filter(!is.na(Date))

# Ensure no remaining NA in Inflation_Rate
if(all(is.na(usd_inflation$Inflation_Rate))) {
  stop("All values in usd_inflation$Inflation_Rate are NA.")
}

# Generate daily dates for USD Inflation
# Assuming 'Inflation_Rate' is monthly, expand to daily by carrying forward the monthly rate
daily_usd_inflation <- usd_inflation %>%
  mutate(
    DailyDate = map(Date, ~ seq(.x, by = "day", length.out = days_in_month(.x)))
  ) %>%
  unnest(DailyDate) %>%
  select(DailyDate, Inflation_Rate) %>%
  rename(Date = DailyDate)

# Clean GDP USD data (Quarterly)
gdp_usd <- clean_data(
  df = gdp_usd, 
  remove_rows = 5, 
  select_cols = 1:2, 
  new_col_names = c("Date", "GDP_USD"), 
  start_date = "2023-12-31", 
  by = "-3 month"
)
rownames(gdp_usd) <- gdp_usd$Date

# Clean USD Interest data
usd_interest <- clean_data(
  df = usd_interest, 
  remove_rows = 5, 
  select_cols = 1:2, 
  new_col_names = c("Date", "INTEREST_USD"), 
  start_date = "2023-12-29", 
  by = "-1 day"
)

# Data Cleaning for EUR Inflation: Ensure 'Inflation_Rate' exists
colnames(eur_inflation) <- c("Date", "Inflation_Rate")

# Plotting Inflation Rates (After Correcting Column Names)
# Before plotting, ensure that 'Inflation_Rate' contains finite values
if(all(is.na(eur_inflation$Inflation_Rate))){
  stop("All values in eur_inflation$Inflation_Rate are NA.")
} else {
  plot(eur_inflation$Inflation_Rate, type = "l", main = "EUR Inflation Rate Over Time", 
       xlab = "Index", ylab = "Inflation Rate")
}

# Create a daily dataset for EUR Inflation
eur_inflation$Date <- as.Date(eur_inflation$Date, format = "%Y-%m-%d")
daily_eur_inflation <- eur_inflation %>%
  mutate(
    DailyDate = map(Date, ~ seq(.x, by = "day", length.out = days_in_month(.x)))
  ) %>%
  unnest(DailyDate) %>%
  select(DailyDate, Inflation_Rate) %>%
  rename(Date = DailyDate)

# Ensure daily_usd_inflation has correct Date type
daily_usd_inflation$Date <- as.Date(daily_usd_inflation$Date, format = "%Y-%m-%d")

# Function to expand quarterly GDP data to daily
expand_quarterly_to_daily <- function(df, value_col) {
  df <- df %>% arrange(Date)
  
  # Ensure the value_col is numeric
  if(!is.numeric(df[[value_col]])) {
    # Attempt to convert to numeric after removing commas and other non-numeric characters
    df[[value_col]] <- as.numeric(gsub(",", "", df[[value_col]]))
    
    # Check for conversion issues
    if(any(is.na(df[[value_col]]))) {
      warning(paste("Conversion to numeric introduced NA values in", value_col))
    }
  }
  
  daily_df <- data.frame(Date = as.Date(character()), Value = numeric())
  
  for(i in 1:(nrow(df)-1)) {
    start_date <- df$Date[i]
    end_date <- df$Date[i+1] - days(1)
    daily_dates <- seq(from = start_date, to = end_date, by = "day")
    
    # Replicate the value_col for each day
    temp_data <- data.frame(Date = daily_dates, Value = rep(df[[value_col]][i], length(daily_dates)))
    
    # Append to daily_df
    daily_df <- bind_rows(daily_df, temp_data)
  }
  
  # Handle the last quarter by extending to the endpoint (assuming 3 months)
  last_start_date <- df$Date[nrow(df)]
  end_date_last <- last_start_date + months(3) - days(1)
  daily_dates_last <- seq(from = last_start_date, to = end_date_last, by = "day")
  
  temp_data_last <- data.frame(Date = daily_dates_last, Value = rep(df[[value_col]][nrow(df)], length(daily_dates_last)))
  
  # Append the last set of data
  daily_df <- bind_rows(daily_df, temp_data_last)
  
  return(daily_df)
}

# Expand GDP EUR and GDP USD to daily data
daily_gdp_eur <- expand_quarterly_to_daily(gdp_eur, "GDP_EUR")
daily_gdp_usd <- expand_quarterly_to_daily(gdp_usd, "GDP_USD")

# Rename columns for clarity 
colnames(eur_usd) <- c("Date", "EUR_USD")
colnames(gdp_eur) <- c("Date", "GDP_EUR")
colnames(gdp_usd) <- c("Date", "GDP_USD")
colnames(eur_interest) <- c("Date", "INTEREST_EUR")
colnames(usd_interest) <- c("Date", "INTEREST_USD")

# Ensure all Date columns are Date type
complete_data_frames <- list(
  eur_usd,
  daily_gdp_eur,
  daily_gdp_usd,
  eur_interest,
  usd_interest,
  daily_eur_inflation,
  daily_usd_inflation
)

complete_data_frames <- lapply(complete_data_frames, function(df) {
  df$Date <- as.Date(df$Date)
  return(df)
})

# Assign back to variables
eur_usd <- complete_data_frames[[1]]
daily_gdp_eur <- complete_data_frames[[2]]
daily_gdp_usd <- complete_data_frames[[3]]
eur_interest <- complete_data_frames[[4]]
usd_interest <- complete_data_frames[[5]]
daily_eur_inflation <- complete_data_frames[[6]]
daily_usd_inflation <- complete_data_frames[[7]]

# Align all datasets to the same date range
start_point <- max(
  min(eur_usd$Date, na.rm = TRUE),
  min(daily_gdp_eur$Date, na.rm = TRUE),
  min(daily_gdp_usd$Date, na.rm = TRUE),
  min(eur_interest$Date, na.rm = TRUE),
  min(usd_interest$Date, na.rm = TRUE),
  min(daily_eur_inflation$Date, na.rm = TRUE),
  min(daily_usd_inflation$Date, na.rm = TRUE)
)

end_point <- min(
  max(eur_usd$Date, na.rm = TRUE),
  max(daily_gdp_eur$Date, na.rm = TRUE),
  max(daily_gdp_usd$Date, na.rm = TRUE),
  max(eur_interest$Date, na.rm = TRUE),
  max(usd_interest$Date, na.rm = TRUE),
  max(daily_eur_inflation$Date, na.rm = TRUE),
  max(daily_usd_inflation$Date, na.rm = TRUE)
)

# Function to filter dataframes based on date range
filter_dates <- function(df, start, end) {
  df %>% filter(Date >= start & Date <= end)
}

# Apply date filtering
eur_usd <- filter_dates(eur_usd, start_point, end_point)
daily_gdp_eur <- filter_dates(daily_gdp_eur, start_point, end_point)
daily_gdp_usd <- filter_dates(daily_gdp_usd, start_point, end_point)
eur_interest <- filter_dates(eur_interest, start_point, end_point)
usd_interest <- filter_dates(usd_interest, start_point, end_point)
daily_eur_inflation <- filter_dates(daily_eur_inflation, start_point, end_point)
daily_usd_inflation <- filter_dates(daily_usd_inflation, start_point, end_point)

# Merge all the data into a single dataframe 
complete_data <- reduce(
  list(
    eur_usd,
    daily_gdp_eur,
    daily_gdp_usd,
    eur_interest,
    usd_interest,
    daily_eur_inflation,
    daily_usd_inflation
  ), 
  full_join, by = "Date"
)

# Handle any duplicated columns due to merging
complete_data <- complete_data %>%
  arrange(Date) %>%
  distinct(Date, .keep_all = TRUE)

# Make column names unique (if necessary)
names(complete_data) <- make.unique(names(complete_data))

# Convert all character columns to numeric
complete_data <- complete_data %>%
  mutate(across(where(is.character), ~ as.numeric(.)))

# Round all numeric columns to 2 decimal places
complete_data <- complete_data %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Rename columns after rounding for clarity
colnames(complete_data) <- c(
  "Date", 
  "EUR_USD", 
  "GDP_EUR", 
  "GDP_USD", 
  "INTEREST_EUR", 
  "INTEREST_USD", 
  "INFLATION_EUR", 
  "INFLATION_USD"
)

# Check the first few rows
print(head(complete_data))

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (2) EXPLORATORY ANALYSIS 
# -----------------------------------------------------------------------------

# Summary Statistics
print(summary(complete_data))

# Correlation Analysis
# Subset numeric columns for correlation
numeric_cols <- complete_data %>% select(-Date)

# Compute correlation matrix
cor_matrix <- cor(numeric_cols, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# General plots 

# Plot EUR/USD Over Time
ggplot(complete_data, aes(x = Date, y = EUR_USD)) +
  geom_line(color = "blue") +
  geom_point(data = complete_data %>% 
               filter(EUR_USD == max(EUR_USD, na.rm = TRUE) | 
                        EUR_USD == min(EUR_USD, na.rm = TRUE)), 
             aes(x = Date, y = EUR_USD), color = "red", size = 3) +
  labs(title = "EUR/USD Exchange Rate Over Time", 
       x = "Date", 
       y = "EUR/USD Exchange Rate") +
  theme_minimal()

# Trends in GDP, Inflation, and Interest Rates
gdp_plot <- ggplot(complete_data, aes(x = Date)) +
  geom_line(aes(y = GDP_EUR, color = "GDP_EUR")) +
  geom_line(aes(y = GDP_USD, color = "GDP_USD")) +
  labs(title = "GDP Trends Over Time", 
       x = "Date", 
       y = "GDP") +
  theme_minimal() +
  scale_color_manual(values = c("GDP_EUR" = "green", "GDP_USD" = "purple"))

inflation_plot <- ggplot(complete_data, aes(x = Date)) +
  geom_line(aes(y = INFLATION_EUR, color = "INFLATION_EUR")) +
  geom_line(aes(y = INFLATION_USD, color = "INFLATION_USD")) +
  labs(title = "Inflation Trends Over Time", 
       x = "Date", 
       y = "Inflation Rate") +
  theme_minimal() +
  scale_color_manual(values = c("INFLATION_EUR" = "orange", "INFLATION_USD" = "brown"))

interest_plot <- ggplot(complete_data, aes(x = Date)) +
  geom_line(aes(y = INTEREST_EUR, color = "INTEREST_EUR")) +
  geom_line(aes(y = INTEREST_USD, color = "INTEREST_USD")) +
  labs(title = "Interest Rate Trends Over Time", 
       x = "Date", 
       y = "Interest Rate") +
  theme_minimal() +
  scale_color_manual(values = c("INTEREST_EUR" = "cyan", "INTEREST_USD" = "magenta"))

# Display the trend plots
print(gdp_plot)
print(inflation_plot)
print(interest_plot)

# Distribution Analysis
# Melt the numeric data for distribution plotting
melted_data <- melt(numeric_cols, variable.name = "Variable", value.name = "Value")

# Plot the distributions
ggplot(melted_data, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~Variable, scales = "free") +
  labs(title = "Variable Distributions", x = "Value", y = "Frequency") +
  theme_minimal()

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (4) MODELS 
# -----------------------------------------------------------------------------
# Scale the data
scaled_data <- preProcess(complete_data[, -1], method = c("center", "scale"))
complete_data_scaled <- predict(scaled_data, complete_data[, -1])
complete_data_scaled <- cbind(Date = complete_data$Date, complete_data_scaled)

# Create a new dataset excluding the EUR_USD column
predictors_data <- complete_data_scaled %>% select(-EUR_USD)

# Linear regression with predictors excluding EUR_USD
lm_model <- lm(EUR_USD ~ ., data = complete_data_scaled %>% select(-Date))
summary(lm_model)

# Evaluate performance
lm_pred <- predict(lm_model, complete_data_scaled %>% select(-EUR_USD))
mse_lm <- mse(complete_data_scaled$EUR_USD, lm_pred)
cat("Linear Regression MSE:", mse_lm, "\n")

# Lasso regression 
# Prepare data for glmnet
X <- as.matrix(predictors_data[, -1])  # Exclude Date
y <- complete_data_scaled$EUR_USD

# Lasso regression
lasso_model <- cv.glmnet(X, y, alpha = 1)
lasso_pred <- predict(lasso_model, s = "lambda.min", newx = X)
mse_lasso <- mse(y, lasso_pred)
cat("Lasso Regression MSE:", mse_lasso, "\n")

# Examine selected features
important_features <- coef(lasso_model, s = "lambda.min")
print(important_features)

# Ridge regression
ridge_model <- cv.glmnet(X, y, alpha = 0)
ridge_pred <- predict(ridge_model, s = "lambda.min", newx = X)
mse_ridge <- mse(y, ridge_pred)
cat("Ridge Regression MSE:", mse_ridge, "\n")

# Examine Ridge coefficients
ridge_coefficients <- coef(ridge_model, s = "lambda.min")
print(ridge_coefficients)

# Residual analysis
residuals_ridge <- y - ridge_pred
plot(residuals_ridge, main = "Residuals for Ridge Regression", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

# Interaction Model
interaction_model <- lm(EUR_USD ~ GDP_USD * INTEREST_USD + GDP_EUR * INTEREST_EUR, data = complete_data_scaled)
summary(interaction_model)

# Fit Decision Tree Model
tree_model <- rpart(EUR_USD ~ ., data = complete_data_scaled %>% select(-Date), method = "anova")

# Predict using the Decision Tree model
tree_pred <- predict(tree_model, complete_data_scaled %>% select(-Date))

# Calculate MSE for Decision Tree
mse_tree <- mse(complete_data_scaled$EUR_USD, tree_pred)
cat("Decision Tree MSE:", mse_tree, "\n")

# General summary: 
# Summarize model performance, including Decision Tree
performance_summary <- data.frame(
  Model = c("Linear Regression", "Lasso Regression", "Ridge Regression", "Decision Tree"),
  MSE = c(mse_lm, mse_lasso, mse_ridge, mse_tree)
)

# Print performance summary
print(performance_summary)

# The best model is the decision tree but it can probably be because of 
# overfitting so let's check if this is the case 
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (4.2) TREE VALIDATION 
# -----------------------------------------------------------------------------
set.seed(123)

# Split data into training and testing sets
train_index <- createDataPartition(complete_data_scaled$EUR_USD, p = 0.8, list = FALSE)
train_data <- complete_data_scaled[train_index, ]
test_data <- complete_data_scaled[-train_index, ]

# Fit Decision Tree on training data
tree_model <- rpart(EUR_USD ~ ., data = train_data %>% select(-Date), method = "anova")

# Predict on test data
tree_test_pred <- predict(tree_model, test_data %>% select(-Date))
test_mse_tree <- mse(test_data$EUR_USD, tree_test_pred)
cat("Decision Tree Test MSE:", test_mse_tree, "\n")

# Prune the tree
pruned_tree <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"])
rpart.plot(pruned_tree)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (5) ENSEMBLE METHODS  
# -----------------------------------------------------------------------------
# Fit Random Forest Model on training data
rf_model <- randomForest(EUR_USD ~ ., data = train_data %>% select(-Date))
rf_test_pred <- predict(rf_model, test_data %>% select(-Date))
rf_test_mse <- mse(test_data$EUR_USD, rf_test_pred)
cat("Random Forest Test MSE:", rf_test_mse, "\n")

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (6) NONLINEAR RELATIONSHIPS 
# -----------------------------------------------------------------------------

# Interaction Model on training data
interaction_model <- lm(EUR_USD ~ GDP_EUR * INTEREST_EUR + GDP_USD * INTEREST_USD, data = train_data)
summary(interaction_model)

# Polynomial Model on training data
polynomial_model <- lm(EUR_USD ~ poly(GDP_EUR, 2) + poly(INTEREST_EUR, 2) +
                         poly(GDP_USD, 2) + poly(INTEREST_USD, 2) +
                         GDP_EUR:INTEREST_EUR + GDP_USD:INTEREST_USD,
                       data = train_data)
summary(polynomial_model)

# Expanded Interaction Model on training data
expanded_interaction_model <- lm(EUR_USD ~ (GDP_EUR + INTEREST_EUR + GDP_USD + INTEREST_USD)^2, 
                                 data = train_data)
summary(expanded_interaction_model)

# Summary of the key features
features_table <- data.frame(
  Feature = c(
    "GDP_EUR", 
    "INTEREST_EUR", 
    "GDP_USD", 
    "INTEREST_USD", 
    "GDP_EUR:INTEREST_EUR", 
    "GDP_USD:INTEREST_USD"
  ),
  Estimate = c(
    coef(interaction_model)["GDP_EUR"], 
    coef(interaction_model)["INTEREST_EUR"], 
    coef(interaction_model)["GDP_USD"], 
    coef(interaction_model)["INTEREST_USD"], 
    coef(interaction_model)["GDP_EUR:INTEREST_EUR"], 
    coef(interaction_model)["GDP_USD:INTEREST_USD"]
  ),
  Interpretation = c(
    "Negative effect on EUR/USD",
    "Positive effect on EUR/USD",
    "Positive effect on EUR/USD",
    "Negative effect on EUR/USD",
    "Negative interaction effect",
    "Positive interaction effect"
  )
)

# Print the table
print(features_table)

# Create a visualization for the feature importance
ggplot(features_table, aes(x = reorder(Feature, Estimate), y = Estimate, fill = Interpretation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance and Effects on EUR/USD",
       x = "Feature",
       y = "Coefficient Estimate") +
  theme_minimal()

# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (7) FINAL MODEL 
# -----------------------------------------------------------------------------
# Fit the final model on training data
final_model <- lm(EUR_USD ~ GDP_EUR + INTEREST_EUR + GDP_USD + INTEREST_USD +
                    GDP_EUR:INTEREST_EUR + GDP_USD:INTEREST_USD, 
                  data = train_data)
summary(final_model)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# (8) DATA ENGINEERING: OUTLIERS 
# -----------------------------------------------------------------------------
# Perform K-Means Clustering
kmeans_result <- kmeans(complete_data_scaled[, -1], centers = 3)
complete_data_scaled$Cluster <- as.factor(kmeans_result$cluster)

# Investigate the composition of clusters
cluster_summary <- complete_data_scaled %>%
  group_by(Cluster) %>%
  summarise(
    Avg_EUR_USD = mean(EUR_USD, na.rm = TRUE),
    Avg_GDP_EUR = mean(GDP_EUR, na.rm = TRUE),
    Avg_GDP_USD = mean(GDP_USD, na.rm = TRUE),
    Avg_INTEREST_EUR = mean(INTEREST_EUR, na.rm = TRUE),
    Avg_INTEREST_USD = mean(INTEREST_USD, na.rm = TRUE),
    Avg_INFLATION_EUR = mean(INFLATION_EUR, na.rm = TRUE),
    Avg_INFLATION_USD = mean(INFLATION_USD, na.rm = TRUE)
  )
print(cluster_summary)

# Visualize clusters
ggplot(complete_data_scaled, aes(x = GDP_USD, y = EUR_USD, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Clusters and EUR/USD", x = "GDP (USD)", y = "EUR/USD Exchange Rate") +
  theme_minimal()

# Pairwise Scatter Plots
ggpairs(
  complete_data_scaled, 
  columns = c("EUR_USD", "GDP_EUR", "GDP_USD", "INTEREST_EUR", "INTEREST_USD", "INFLATION_EUR", "INFLATION_USD"), 
  aes(color = Cluster, alpha = 0.7)
)

# Cluster Profiles - Heatmap
cluster_summary_long <- melt(cluster_summary, id.vars = "Cluster")

ggplot(cluster_summary_long, aes(x = variable, y = Cluster, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Cluster Profiles", x = "Variable", y = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analyze Cluster Significance
anova_results <- aov(EUR_USD ~ Cluster, data = complete_data_scaled)
summary(anova_results)

# ------------------------------------------------------------------------------
