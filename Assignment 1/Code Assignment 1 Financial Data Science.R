## Federica Malamisura
## FA 582-A Foundation of Financial Data Science
## Professor Dragos Bozdog
## Assignment 1
## September 24th, 2024


# PROBLEM 1

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(openxlsx)
library(scales)

# Set the working directory (modify the path according to your folders)
setwd("/Users/federicamalamisura/Desktop/Assignment 1 R 09:24 /HW1_F24")

# Check the directory
getwd()

# Read the datasets for each borough, skipping the first 4 rows
bronx <- read.xlsx("rollingsales_bronx.xlsx", sheet = 1, startRow = 5)
brooklyn <- read.xlsx("rollingsales_brooklyn.xlsx", sheet = 1, startRow = 5)
manhattan <- read.xlsx("rollingsales_manhattan.xlsx", sheet = 1, startRow = 5)
queens <- read.xlsx("rollingsales_queens.xlsx", sheet = 1, startRow = 5)
staten_island <- read.xlsx("rollingsales_statenisland.xlsx", sheet = 1, startRow = 5)

# Clean column names: remove spaces, make lowercase, and replace with dots
colnames(bronx) <- make.names(tolower(colnames(bronx)), unique = TRUE)
colnames(brooklyn) <- make.names(tolower(colnames(brooklyn)), unique = TRUE)
colnames(manhattan) <- make.names(tolower(colnames(manhattan)), unique = TRUE)
colnames(queens) <- make.names(tolower(colnames(queens)), unique = TRUE)
colnames(staten_island) <- make.names(tolower(colnames(staten_island)), unique = TRUE)

# Add a column for the borough
bronx$borough <- "Bronx"
brooklyn$borough <- "Brooklyn"
manhattan$borough <- "Manhattan"
queens$borough <- "Queens"
staten_island$borough <- "Staten Island"

# Combine all datasets into one
nyc_sales <- rbind(bronx, brooklyn, manhattan, queens, staten_island)

# Convert 'SALE.DATE' to Date format
nyc_sales$sale.date <- as.Date(nyc_sales$sale.date, origin = "1899-12-30")

# We will check if we can delete some columns  
count_missing_values <- function(dataframe, column_name) {
  # Check if the column exists in the dataframe
  if (!(column_name %in% colnames(dataframe))) {
    stop("Column does not exist in the dataframe.")
  }
  
  # Count the number of missing values (NA) in the specified column
  missing_count <- sum(is.na(dataframe[[column_name]]))
  
  # Return the number of missing values
  return(missing_count)
}

missing_values_apart <- count_missing_values(nyc_sales, "apart.ment.number")
print(missing_values_apart)

missing_values_in_easement <- count_missing_values(nyc_sales, "ease.ment")
print(missing_values_in_easement)

#Thus we can delete the colum corresponding to basement, but we can't delete the column related to apartment number 
nyc_sales$ease.ment <- NULL

# Clean the columns of interest and convert them to numeric
nyc_sales$land.square.feet <- as.numeric(gsub("[^0-9]", "", nyc_sales$land.square.feet))
nyc_sales$gross.square.feet <- as.numeric(gsub("[^0-9]", "", nyc_sales$gross.square.feet))
nyc_sales$sale.price <- as.numeric(gsub("[^0-9]", "", nyc_sales$sale.price))
nyc_sales$year.built <- as.numeric(nyc_sales$year.built)

# Remove rows where sale price is missing or zero
nyc_sales <- nyc_sales[!is.na(nyc_sales$sale.price) & nyc_sales$sale.price > 0, ]

# Data check:Display unique values of the building.class.category column to confirm matches:
unique(nyc_sales$building.class.category)

# Remove extra spaces: Trim extra spaces from the values in building.class.category:
nyc_sales$building.class.category <- trimws(nyc_sales$building.class.category)

# Example filter: Try filtering with a single category to see if you get results:
nyc_res_test <- nyc_sales %>% filter(building.class.category == "01  ONE FAMILY HOMES")
nyc_res_test2 <- nyc_sales %>% filter(building.class.category == "02  TWO FAMILY HOMES")

# Check data type: Ensure that building.class.category is of type character:
str(nyc_sales)


# Select relevant building class categories
# 1-, 2-, 3-family homes, coops, and condos
residential_categories <- c( "01  ONE FAMILY HOMES",
                             "02  TWO FAMILY HOMES",
                             "03  THREE FAMILY HOMES",
                             "09  COOPS - WALKUP APARTMENTS",
                             "10  COOPS - ELEVATOR APARTMENTS",
                             "12  CONDOS - WALKUP APARTMENTS",
                             "13  CONDOS - ELEVATOR APARTMENTS")

# Filter data for the selected categories
nyc_residential <- nyc_sales %>% filter(building.class.category %in% residential_categories)


# Extract year and month from 'SALE.DATE' using base R functions
nyc_residential$sale.month <- as.numeric(format(nyc_residential$sale.date, "%m"))
nyc_residential$sale.year <- as.numeric(format(nyc_residential$sale.date, "%Y"))

# Visualizations
# Histogram of Sale Prices
ggplot(nyc_residential, aes(x = sale.price)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
  scale_x_continuous(labels = comma) +
  facet_wrap(~building.class.category) +
  labs(title = "Distribution of Sale Prices by Building Category",
       x = "Sale Price",
       y = "Count") +
  theme_minimal()

# Boxplot of Sale Prices by Borough
ggplot(nyc_residential, aes(x = borough, y = sale.price)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "Sale Prices by Borough",
       x = "Borough",
       y = "Sale Price") +
  theme_minimal()

# Scatterplot of Sale Prices Over Time
ggplot(nyc_residential, aes(x = sale.date, y = sale.price, color = borough)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(labels = comma) +
  labs(title = "Sale Prices Over Time",
       x = "Date",
       y = "Sale Price") +
  theme_minimal()

# Summary Statistics
summary_stats <- nyc_residential %>%
  group_by(building.class.category, borough) %>%
  summarise(
    count = n(),
    mean_price = mean(sale.price, na.rm = TRUE),
    median_price = median(sale.price, na.rm = TRUE),
    sd_price = sd(sale.price, na.rm = TRUE)
  )

print(summary_stats)




# PROBLEM 2

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set the directory
setwd("/Users/federicamalamisura/Desktop/Assignment 1 R 09:24 /HW1_F24")

# Check the directory
getwd()

# Read the datasets using base R
nyt1 <- read.csv("nyt1.csv")
nyt2 <- read.csv("nyt2.csv")
nyt3 <- read.csv("nyt3.csv")

# Function to create age_group variable using base R
create_age_group <- function(data) {
  data$age_group <- cut(data$Age,
                        breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf),
                        labels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
                        right = TRUE)
  return(data)
}

# Apply the function to each dataset
nyt1 <- create_age_group(nyt1)
nyt2 <- create_age_group(nyt2)
nyt3 <- create_age_group(nyt3)

# Combine the datasets
nyt_all <- rbind(nyt1, nyt2, nyt3)

# Convert Gender and Signed_In to factors
nyt_all$Gender <- factor(nyt_all$Gender, levels = c(0,1), labels = c("Female", "Male"))
nyt_all$Signed_In <- factor(nyt_all$Signed_In, levels = c(0,1), labels = c("Not Signed In", "Signed In"))

# Calculate CTR
nyt_all$CTR <- ifelse(nyt_all$Impressions > 0, nyt_all$Clicks / nyt_all$Impressions, 0)

# Function to segment users
segment_users <- function(data) {
  data$segment <- ifelse(data$Clicks == 0, "No Clicks",
                         ifelse(data$Clicks >= 1 & data$Clicks <= 3, "Low Clicks",
                                ifelse(data$Clicks > 3, "High Clicks", NA)))
  return(data)
}

# Apply the function to the combined dataset
nyt_all <- segment_users(nyt_all)

# Explore the data and make visual and quantitative comparisons across user segments/demographics

## Comparison between <20-year-old males versus <20-year-old females

# Subset data for users under 20
data_under20 <- subset(nyt_all, age_group == "<20")

# Plot Impressions for <20 males vs females
ggplot(data_under20, aes(x = Gender, y = Impressions, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~Signed_In) +
  theme_minimal() +
  labs(title = "Impressions by Gender for Users Under 20",
       x = "Gender",
       y = "Impressions")

# Plot CTR for <20 males vs females
ggplot(data_under20, aes(x = Gender, y = CTR, fill = Gender)) +
  geom_boxplot() +
  facet_wrap(~Signed_In) +
  theme_minimal() +
  labs(title = "CTR by Gender for Users Under 20",
       x = "Gender",
       y = "Click-Through Rate")

# Quantitative comparisons
summary_stats_under20 <- data_under20 %>%
  group_by(Gender, Signed_In) %>%
  summarise(
    Mean_Impressions = mean(Impressions),
    Median_Impressions = median(Impressions),
    Mean_CTR = mean(CTR),
    Median_CTR = median(CTR)
  )

print(summary_stats_under20)

## Comparison between logged-in versus not logged-in users across all age groups

# Plot Impressions by Signed_In status
ggplot(nyt_all, aes(x = Signed_In, y = Impressions, fill = Signed_In)) +
  geom_boxplot() +
  facet_wrap(~age_group) +
  theme_minimal() +
  labs(title = "Impressions by Signed-In Status Across Age Groups",
       x = "Signed-In Status",
       y = "Impressions")

# Plot CTR by Signed_In status
ggplot(nyt_all, aes(x = Signed_In, y = CTR, fill = Signed_In)) +
  geom_boxplot() +
  facet_wrap(~age_group) +
  theme_minimal() +
  labs(title = "CTR by Signed-In Status Across Age Groups",
       x = "Signed-In Status",
       y = "Click-Through Rate")

# Quantitative comparisons
summary_stats_signed_in <- nyt_all %>%
  group_by(Signed_In, age_group) %>%
  summarise(
    Mean_Impressions = mean(Impressions),
    Median_Impressions = median(Impressions),
    Mean_CTR = mean(CTR),
    Median_CTR = median(CTR)
  )

print(summary_stats_signed_in)

