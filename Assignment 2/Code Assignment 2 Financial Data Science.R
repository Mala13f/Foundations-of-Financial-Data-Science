## ASSIGNMENT 2
## Federica Malamisura
## FA 582-A Foundation of Financial Data Science
## Professor Dragos Bozdog

## PROBLEM 1

# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(proxy)
library(cluster)
library(arules)

## Scrape the S&P 500 companies table from Wikipedia

# Specify the URL
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# Read the HTML content from the webpage (+ error handling)
webpage <- tryCatch({
  read_html(url)
}, error = function(e) {
  message("Error: Unable to scrape the webpage")
  return(NULL)
})

# Use CSS selectors to extract the table
tables <- html_nodes(webpage, "table")

# The first table contains the S&P 500 companies
sp500_table <- html_table(tables[1], fill = TRUE)[[1]]

# Check the column names
colnames(sp500_table)

# Clean the column names
colnames(sp500_table) <- c("Symbol", "Security", "GICS_Sector",
                           "GICS_Sub_Industry", "Headquarters_Location", "Date_added", "CIK", "Founded")

# Check the structure of the table
str(sp500_table)

# Convert columns to their appropriate formats
sp500_table$GICS_Sector <- as.factor(sp500_table$GICS_Sector)
sp500_table$GICS_Sub_Industry <- as.factor(sp500_table$GICS_Sub_Industry)
sp500_table$Date_added <- as.Date(sp500_table$Date_added, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")) #The table in the webpage could change
sp500_table$CIK <- sprintf("%010d", sp500_table$CIK)  # Keep CIK as character with leading zeros
sp500_table$Founded <- as.character(sp500_table$Founded) #Because it has more complex data not only the year

# View the structure after conversions
str(sp500_table)

# Check for missing values in each column
colSums(is.na(sp500_table))

# View the first few rows
head(sp500_table)


## Perform exploratory data analysis and report summary statistics

# 1. Summary Statistics for the Dataset

# Summary for the Date_added (Date type)
summary(sp500_table$Date_added)

# Summary for categorical columns
summary(sp500_table$GICS_Sector)
summary(sp500_table$GICS_Sub_Industry)
summary(sp500_table$Headquarters_Location)

# 2. Distribution of Companies by Sector (GICS_Sector)

# Count companies by GICS Sector
sector_counts <- sp500_table %>%
  group_by(GICS_Sector) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Print the sector counts
print(sector_counts)

# Plot the distribution of companies by GICS_Sector
ggplot(sector_counts, aes(x = reorder(GICS_Sector, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Number of Companies per GICS Sector",
       x = "GICS Sector", y = "Count") +
  theme_minimal()

# 3. Distribution of Companies by Sub-Industry (GICS_Sub_Industry)

# Count companies by GICS Sub-Industry
sub_industry_counts <- sp500_table %>%
  group_by(GICS_Sub_Industry) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Print the sub-industry counts
print(sub_industry_counts)

# Plot the distribution of companies by GICS_Sub_Industry (Top 10 for clarity)
top_10_sub_industries <- head(sub_industry_counts, 10)

ggplot(top_10_sub_industries, aes(x = reorder(GICS_Sub_Industry, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 GICS Sub-Industries",
       x = "GICS Sub-Industry", y = "Count") +
  theme_minimal()

# 4. Distribution of Companies by Headquarters Location

# Count companies by headquarters location
headquarters_counts <- sp500_table %>%
  group_by(Headquarters_Location) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Print the top 10 headquarters locations
print(head(headquarters_counts, 10))

# Plot the top 10 headquarters locations
top_10_headquarters <- head(headquarters_counts, 10)

ggplot(top_10_headquarters, aes(x = reorder(Headquarters_Location, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 Headquarters Locations",
       x = "Headquarters Location", y = "Count") +
  theme_minimal()

# 5. Analysis of the Founded Column

# Extract the first year from the "Founded" column (if it's in complex format like "2013 (1888)")
sp500_table$Founded_cleaned <- as.numeric(str_extract(sp500_table$Founded, "\\d{4}"))

# Summary statistics for the Founded year
summary(sp500_table$Founded_cleaned)

# Plot the distribution of Founded years (after cleaning)
ggplot(sp500_table, aes(x = Founded_cleaned)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Companies by Year Founded",
       x = "Year Founded", y = "Frequency") +
  theme_minimal()

# 6. Correlation between Date Added and Founded Year

# Scatter plot showing the relationship between Date_added and Founded_cleaned
ggplot(sp500_table, aes(x = Founded_cleaned, y = Date_added)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Date Added vs Year Founded",
       x = "Year Founded", y = "Date Added to S&P 500") +
  theme_minimal()

## PROBLEM 2

# Set the working directory if needed
setwd("/Volumes/FM/Subjects/Foundations of Financial Data Science/Assignment 2/HW2_data")

# Read the CSV files
fundamentals <- read.csv("fundamentals.csv")
securities <- read.csv("securities.csv")

# Filter for the year 2013 in the fundamentals dataset
fundamentals_2013 <- fundamentals %>% filter(`For.Year` == 2013)

# Randomly select 100 tickers from the filtered data
set.seed(123)
selected_tickers <- sample(unique(fundamentals_2013$`Ticker.Symbol`), 100)

# Filter the fundamentals data to include only selected tickers
subset_fundamentals <- fundamentals_2013 %>%
  filter(`Ticker.Symbol` %in% selected_tickers)

# Select quantitative variables
quantitative_vars <- c("Pre.Tax.ROE", "After.Tax.ROE", "Total.Revenue", "Net.Income", "Operating.Margin",
                       "Pre.Tax.Margin", "Gross.Profit", "Profit.Margin", "Quick.Ratio",
                       "Total.Assets", "Total.Liabilities", "Earnings.Per.Share")

# Filter the data for these quantitative variables
selected_quantitative <- subset_fundamentals %>%
  select(`Ticker.Symbol`, all_of(quantitative_vars))

# Replace NA with 0 in the selected_quantitative data frame
quantitative_clean <- selected_quantitative %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Ensure no duplicated tickers
quantitative_clean <- quantitative_clean[!duplicated(quantitative_clean$`Ticker.Symbol`), ]

# Normalize the quantitative variables (excluding Ticker.Symbol for scaling)
normalized_data <- as.data.frame(scale(quantitative_clean[, -1]))

# Add the unique ticker symbols back as row names
rownames(normalized_data) <- quantitative_clean$`Ticker.Symbol`

## Now categorical data from `securities_100`
securities_100 <- securities %>%
  filter(`Ticker.symbol` %in% selected_tickers) %>%
  select(GICS.Sector, GICS.Sub.Industry, Address.of.Headquarters) %>%
  mutate(across(everything(), as.factor))

rownames(securities_100) <- selected_tickers

## Lp-Norm Distance Calculations
# Generic Lp-Norm Distance Function

calculate_lp_distance <- function(data, p) {
  lp_norm <- function(x, y, p) {
    sum(abs(x - y)^p)^(1/p)
  }
  
  num_ticker <- nrow(data)
  distance_matrix <- matrix(0, nrow = num_ticker, ncol = num_ticker)
  
  # Calculate pairwise distances for all tickers
  for (i in 1:(num_ticker - 1)) {
    for (j in (i + 1):num_ticker) {
      distance_matrix[i, j] <- lp_norm(as.numeric(data[i, ]), as.numeric(data[j, ]), p)
      distance_matrix[j, i] <- distance_matrix[i, j]  # Symmetric matrix
    }
  }
  
  # Convert the distance matrix into a data frame with row and column names (tickers)
  dist_df <- as.data.frame(as.table(distance_matrix))
  
  # Assign the correct ticker symbols as names
  colnames(dist_df) <- c("Ticker1", "Ticker2", paste0("L_", p, "_Distance"))
  dist_df$Ticker1 <- rownames(data)[dist_df$Ticker1]
  dist_df$Ticker2 <- rownames(data)[dist_df$Ticker2]
  
  # Since the distance is symmetrical we have to remove the duplicates
  dist_df <- dist_df[!duplicated(t(apply(dist_df[1:2], 1, sort))), ]
  
  return(dist_df)
}

# Run Lp-Norm Distance functions for p = 1, 2, 3, 10
l1_dist_df <- calculate_lp_distance(normalized_data, p = 1)
l2_dist_df <- calculate_lp_distance(normalized_data, p = 2)
l3_dist_df <- calculate_lp_distance(normalized_data, p = 3)
l10_dist_df <- calculate_lp_distance(normalized_data, p = 10)

# Clean and sort the distance dataframes (remove diagonal elements where Ticker1 == Ticker2)
clean_distances <- function(dist_df) {
  dist_df <- dist_df[dist_df$Ticker1 != dist_df$Ticker2, ]
  dist_df <- dist_df[order(dist_df[, 3]), ]
  return(dist_df)
}

l1_dist_df <- clean_distances(l1_dist_df)
l2_dist_df <- clean_distances(l2_dist_df)
l3_dist_df <- clean_distances(l3_dist_df)
l10_dist_df <- clean_distances(l10_dist_df)

# Top and Bottom 10 for L1, L2, L3, L10
list(
  L1_Top10 = head(l1_dist_df, 10),
  L1_Bottom10 = tail(l1_dist_df, 10),
  L2_Top10 = head(l2_dist_df, 10),
  L2_Bottom10 = tail(l2_dist_df, 10),
  L3_Top10 = head(l3_dist_df, 10),
  L3_Bottom10 = tail(l3_dist_df, 10),
  L10_Top10 = head(l10_dist_df, 10),
  L10_Bottom10 = tail(l10_dist_df, 10)
)


## Weighted Minkowski Distance Function

calculate_weighted_minkowski <- function(data, p, weights) {
  minkowski_dist <- function(x, y, p, weights) {
    sum(weights * abs(x - y)^p)^(1/p)
  }
  
  num_tickers <- nrow(data)
  distance_matrix <- matrix(0, nrow = num_tickers, ncol = num_tickers)
  
  for (i in 1:(num_tickers - 1)) {
    for (j in (i + 1):num_tickers) {
      distance_matrix[i, j] <- minkowski_dist(as.numeric(data[i, ]), as.numeric(data[j, ]), p, weights)
      distance_matrix[j, i] <- distance_matrix[i, j]  # Symmetric matrix
    }
  }
  
  dist_df <- as.data.frame(as.table(as.matrix(distance_matrix)))
  colnames(dist_df) <- c("Ticker1", "Ticker2", paste0("Weighted_Minkowski_L_", p, "_Distance"))
  
  # Assign the correct ticker symbols as names
  colnames(dist_df) <- c("Ticker1", "Ticker2", paste0("L_", p, "_Distance"))
  dist_df$Ticker1 <- rownames(data)[dist_df$Ticker1]
  dist_df$Ticker2 <- rownames(data)[dist_df$Ticker2]
  
  # Since the distance is symmetrical we have to remove the duplicates
  dist_df <- dist_df[!duplicated(t(apply(dist_df[1:2], 1, sort))), ]
  
  return(dist_df)
}


# Assign weights based on variance of each variable
feature_variances <- apply(normalized_data, 2, var)
weights <- feature_variances / sum(feature_variances)  # Normalize weights

# Calculate weighted Minkowski distance for p = 2
weighted_minkowski_df <- calculate_weighted_minkowski(normalized_data, p = 2, weights)
weighted_minkowski_df <- clean_distances(weighted_minkowski_df)

# Get top and bottom 10 for weighted Minkowski
list(
  Weighted_Minkowski_Top10 = head(weighted_minkowski_df, 10),
  Weighted_Minkowski_Bottom10 = tail(weighted_minkowski_df, 10)
)



## Match-Based Similarity Computation (using equi-depth buckets)

# Function to compute match-based similarity using equi-depth buckets
calculate_match_based_similarity <- function(quantitative_data, num_buckets = 3) {
  # Determine the number of rows (tickers) and columns (features)
  num_tickers <- nrow(quantitative_data)
  num_features <- ncol(quantitative_data)
  
  # Initialize an empty similarity matrix
  similarity_matrix <- matrix(0, nrow = num_tickers, ncol = num_tickers)
  
  # For each feature, divide it into equi-depth buckets
  for (feature_idx in 1:num_features) {
    # Get the current feature data
    feature_data <- quantitative_data[, feature_idx]
    
    # Create equi-depth buckets (cut divides the data into 'num_buckets' buckets)
    buckets <- cut(feature_data, breaks = num_buckets, include.lowest = TRUE, labels = FALSE)
    
    # For each pair of tickers, check if they fall into the same bucket
    for (i in 1:(num_tickers - 1)) {
      for (j in (i + 1):num_tickers) {
        if (buckets[i] == buckets[j]) {
          similarity_matrix[i, j] <- similarity_matrix[i, j] + 1
          similarity_matrix[j, i] <- similarity_matrix[i, j]  # Symmetric matrix
        }
      }
    }
  }
  
  # Normalize the similarity matrix by the number of features
  similarity_matrix <- similarity_matrix / num_features
  
  # Convert the similarity matrix into a data frame for easier analysis
  sim_df <- as.data.frame(as.table(as.matrix(similarity_matrix)))
  colnames(sim_df) <- c("Ticker1", "Ticker2", "Match_Based_Similarity")
  
  sim_df$Ticker1 <- rownames(quantitative_data)[sim_df$Ticker1]
  sim_df$Ticker2 <- rownames(quantitative_data)[sim_df$Ticker2]
  
  # Since the distance is symmetrical we have to remove the duplicates
  sim_df <- sim_df[!duplicated(t(apply(sim_df[1:2], 1, sort))), ]
  
  return(sim_df)
}

# Apply match-based similarity calculation on the normalized quantitative data
match_based_similarity_df <- calculate_match_based_similarity(normalized_data, num_buckets = 3)

# Clean and sort the match-based similarity data
match_based_similarity_df <- match_based_similarity_df[match_based_similarity_df$Ticker1 != match_based_similarity_df$Ticker2, ]
match_based_similarity_df <- match_based_similarity_df[order(match_based_similarity_df$Match_Based_Similarity, decreasing = TRUE), ]

# Top and Bottom 10 for Match-Based Similarity
list(
  Match_Based_Top10 = head(match_based_similarity_df, 10),
  Match_Based_Bottom10 = tail(match_based_similarity_df, 10)
)


## Mahalanobis Distance

calculate_mahalanobis_distance <- function(data) {
  cov_matrix <- cov(data)  # Covariance matrix of the dataset
  inv_cov_matrix <- solve(cov_matrix)  # Inverse of the covariance matrix
  
  num_tickers <- nrow(data)
  mahalanobis_distances <- matrix(0, nrow = num_tickers, ncol = num_tickers)
  
  for (i in 1:(num_tickers - 1)) {
    for (j in (i + 1):num_tickers) {
      diff <- as.numeric(data[i, ] - data[j, ])
      mahalanobis_distances[i, j] <- sqrt(t(diff) %*% inv_cov_matrix %*% diff)
      mahalanobis_distances[j, i] <- mahalanobis_distances[i, j]  # Symmetric matrix
    }
  }
  
  dist_df <- as.data.frame(as.table(as.matrix(mahalanobis_distances)))
  colnames(dist_df) <- c("Ticker1", "Ticker2", "Mahalanobis_Distance")
  
  dist_df$Ticker1 <- rownames(data)[dist_df$Ticker1]
  dist_df$Ticker2 <- rownames(data)[dist_df$Ticker2]

  # Since the distance is symmetrical we have to remove the duplicates
  dist_df <- dist_df[!duplicated(t(apply(dist_df[1:2], 1, sort))), ]
  
  return(dist_df)
}

# Calculate Mahalanobis distance
mahalanobis_dist_df <- calculate_mahalanobis_distance(normalized_data)
mahalanobis_dist_df <- clean_distances(mahalanobis_dist_df)

# Top and Bottom 10 for Mahalanobis Distance
list(
  Mahalanobis_Top10 = head(mahalanobis_dist_df, 10),
  Mahalanobis_Bottom10 = tail(mahalanobis_dist_df, 10)
)


## Categorical Similarity Calculations

## Similarity: Overlap Measure
calculate_overlap_similarity <- function(cat_data) {
  num_tickers <- nrow(cat_data)
  similarity_matrix <- matrix(0, nrow = num_tickers, ncol = num_tickers)
  
  for (i in 1:(num_tickers - 1)) {
    for (j in (i + 1):num_tickers) {
      similarity_matrix[i, j] <- sum(cat_data[i, ] == cat_data[j, ]) / ncol(cat_data)
      similarity_matrix[j, i] <- similarity_matrix[i, j]  # Symmetric matrix
    }
  }
  
  # Convert the similarity matrix into a data frame for easier analysis
  sim_df <- as.data.frame(as.table(as.matrix(similarity_matrix)))
  colnames(sim_df) <- c("Ticker1", "Ticker2", "Overlap_Similarity")
  
  sim_df$Ticker1 <- rownames(cat_data)[sim_df$Ticker1]
  sim_df$Ticker2 <- rownames(cat_data)[sim_df$Ticker2]
  
  # Since the distance is symmetrical we have to remove the duplicates
  sim_df <- sim_df[!duplicated(t(apply(sim_df[1:2], 1, sort))), ]
  
  return(sim_df)
}

# Calculate the overlap similarity
overlap_sim_df <- calculate_overlap_similarity(securities_100)

# Clean and sort overlap similarity data
overlap_sim_df <- overlap_sim_df[overlap_sim_df$Ticker1 != overlap_sim_df$Ticker2, ]
overlap_sim_df <- overlap_sim_df[order(overlap_sim_df$Overlap_Similarity, decreasing = TRUE), ]

# Top and Bottom 10 for Overlap Similarity
list(
  Overlap_Top10 = head(overlap_sim_df, 10),
  Overlap_Bottom10 = tail(overlap_sim_df, 10)
)


## Similarity: Inverse Frequency Measure

calculate_inverse_frequency_similarity <- function(cat_data) {
  num_tickers <- nrow(cat_data)
  num_features <- ncol(cat_data)
  
  # Calculate frequency of each category in each feature
  frequencies <- apply(cat_data, 2, function(column) {
    table(column) / length(column)
  })
  
  similarity_matrix <- matrix(0, nrow = num_tickers, ncol = num_tickers)
  
  for (i in 1:(num_tickers - 1)) {
    for (j in (i + 1):num_tickers) {
      inverse_freq_sum <- 0
      for (k in 1:num_features) {
        if (cat_data[i, k] == cat_data[j, k]) {
          inverse_freq_sum <- inverse_freq_sum + (1 / frequencies[[k]][cat_data[i, k]])
        }
      }
      similarity_matrix[i, j] <- inverse_freq_sum / num_features
      similarity_matrix[j, i] <- similarity_matrix[i, j]
    }
  }
  
  # Convert the similarity matrix into a data frame for easier analysis
  sim_df <- as.data.frame(as.table(as.matrix(similarity_matrix)))
  colnames(sim_df) <- c("Ticker1", "Ticker2", "Inverse_Frequency_Similarity")
  
  sim_df$Ticker1 <- rownames(cat_data)[sim_df$Ticker1]
  sim_df$Ticker2 <- rownames(cat_data)[sim_df$Ticker2]
  
  # Since the distance is symmetrical we have to remove the duplicates
  sim_df <- sim_df[!duplicated(t(apply(sim_df[1:2], 1, sort))), ]
  
  return(sim_df)
}

# Calculate the inverse frequency similarity
inverse_freq_sim_df <- calculate_inverse_frequency_similarity(securities_100)

# Clean and sort inverse frequency similarity data
inverse_freq_sim_df <- inverse_freq_sim_df[inverse_freq_sim_df$Ticker1 != inverse_freq_sim_df$Ticker2, ]
inverse_freq_sim_df <- inverse_freq_sim_df[order(inverse_freq_sim_df$Inverse_Frequency_Similarity, decreasing = TRUE), ]

# Top and Bottom 10 for Inverse Frequency Similarity
list(
  Inverse_Frequency_Top10 = head(inverse_freq_sim_df, 10),
  Inverse_Frequency_Bottom10 = tail(inverse_freq_sim_df, 10)
)


## Similarity: Goodall Measure

calculate_goodall_similarity <- function(cat_data) {
  num_tickers <- nrow(cat_data)
  num_features <- ncol(cat_data)
  
  # Calculate category frequencies
  frequencies <- apply(cat_data, 2, function(column) {
    table(column) / length(column)
  })
  
  similarity_matrix <- matrix(0, nrow = num_tickers, ncol = num_tickers)
  
  for (i in 1:(num_tickers - 1)) {
    for (j in (i + 1):num_tickers) {
      goodall_sum <- 0
      for (k in 1:num_features) {
        if (cat_data[i, k] == cat_data[j, k]) {
          freq <- frequencies[[k]][cat_data[i, k]]
          goodall_sum <- goodall_sum + (1 - freq^2)
        }
      }
      similarity_matrix[i, j] <- goodall_sum / num_features
      similarity_matrix[j, i] <- similarity_matrix[i, j]
    }
  }
  
  # Convert the similarity matrix into a data frame for easier analysis
  sim_df <- as.data.frame(as.table(as.matrix(similarity_matrix)))
  colnames(sim_df) <- c("Ticker1", "Ticker2", "Goodall_Similarity")
  
  sim_df$Ticker1 <- rownames(cat_data)[sim_df$Ticker1]
  sim_df$Ticker2 <- rownames(cat_data)[sim_df$Ticker2]
  
  # Since the distance is symmetrical we have to remove the duplicates
  sim_df <- sim_df[!duplicated(t(apply(sim_df[1:2], 1, sort))), ]
  
  return(sim_df)
}

# Calculate the Goodall similarity
goodall_sim_df <- calculate_goodall_similarity(securities_100)

# Clean and sort Goodall similarity data
goodall_sim_df <- goodall_sim_df[goodall_sim_df$Ticker1 != goodall_sim_df$Ticker2, ]
goodall_sim_df <- goodall_sim_df[order(goodall_sim_df$Goodall_Similarity, decreasing = TRUE), ]

# Top and Bottom 10 for Goodall Similarity
list(
  Goodall_Top10 = head(goodall_sim_df, 10),
  Goodall_Bottom10 = tail(goodall_sim_df, 10)
)


## Overall Similarity between tickers (Mixed Data with λ value)

# Function to calculate overall similarity
calculate_overall_similarity <- function(quantitative_dist_df, categorical_sim_df, lambda) {
  # Merge the quantitative distance and categorical similarity dataframes
  merged_df <- merge(quantitative_dist_df, categorical_sim_df, by = c("Ticker1", "Ticker2"))
  
  # Calculate the overall similarity
  merged_df$Overall_Similarity <- lambda * merged_df$L_2_Distance + (1 - lambda) * (1 - merged_df$Overlap_Similarity)
  
  # Sort by Overall Similarity
  merged_df <- merged_df[order(merged_df$Overall_Similarity), ]
  
  merged_df$Ticker1 <- rownames(data)[merged_df$Ticker1]
  merged_df$Ticker2 <- rownames(data)[merged_df$Ticker2]
  
  # Since the distance is symmetrical we have to remove the duplicates
  merged_df <- merged_df[!duplicated(t(apply(merged_df[1:2], 1, sort))), ]
  
  return(merged_df)
}

# Choose a lambda value (e.g., 0.5 for equal weighting)
lambda <- 0.5

# Calculate overall similarity using L2 distance and Overlap similarity
overall_similarity_df <- calculate_overall_similarity(l2_dist_df, overlap_sim_df, lambda)

# Top and Bottom 10 for Overall Similarity
list(
  Overall_Top10 = head(overall_similarity_df, 10),
  Overall_Bottom10 = tail(overall_similarity_df, 10)
)


## Overall Normalized Similarity between tickers (Mixed Data)

# Function to normalize a vector using min-max scaling
normalize_min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Function to calculate overall normalized similarity
calculate_overall_normalized_similarity <- function(quantitative_dist_df, categorical_sim_df, lambda) {
  # Normalize the L2 distances
  quantitative_dist_df$L_2_Distance <- normalize_min_max(quantitative_dist_df$L_2_Distance)
  
  # Merge the normalized quantitative distance and categorical similarity dataframes
  merged_df <- merge(quantitative_dist_df, categorical_sim_df, by = c("Ticker1", "Ticker2"))
  
  # Calculate the overall normalized similarity
  merged_df$Overall_Normalized_Similarity <- lambda * merged_df$L_2_Distance + (1 - lambda) * (1 - merged_df$Overlap_Similarity)
  
  # Sort by Overall Normalized Similarity
  merged_df <- merged_df[order(merged_df$Overall_Normalized_Similarity), ]
  
  # Since the distance is symmetrical we have to remove the duplicates
  merged_df <- merged_df[!duplicated(t(apply(merged_df[1:2], 1, sort))), ]
  
  return(merged_df)
}

# Choose a lambda value (e.g., 0.5 for equal weighting)
lambda <- 0.5

# Calculate overall normalized similarity using L2 distance and Overlap similarity
overall_normalized_similarity_df <- calculate_overall_normalized_similarity(l2_dist_df, overlap_sim_df, lambda)

# Top and Bottom 10 for Overall Normalized Similarity
list(
  Overall_Normalized_Top10 = head(overall_normalized_similarity_df, 10),
  Overall_Normalized_Bottom10 = tail(overall_normalized_similarity_df, 10)
)
