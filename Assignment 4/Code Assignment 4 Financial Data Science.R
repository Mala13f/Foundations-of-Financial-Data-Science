## Foundations of Financial Data Science FA-582
## Assignment 4
## Federica Malamisura


setwd("~/Desktop/ASSIGNMENT 4/HW4_data")
getwd()


## Problem 1

# Load required libraries
library(tidyverse)
library(tree)  # for cv.tree
library(MASS)  # required for some tree functions

# Load data
data <- read.csv('OJ.csv')

str(data)

# a) Create a training set containing a random sample of 800 observations, and a test set containing the remaining observations.

# Set seed for reproducibility
set.seed(123)

# Create training index
train_index <- sample(1:nrow(data), 800)

# Split data
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# b) Fit a tree to the training data, with Purchase as the response and the other variables as
# predictors. Use the summary() function to produce summary statistics about the tree,
# and describe the results obtained. What is the training error rate? How many terminal
# nodes does the tree have?

train_data <- train_data %>%
  mutate_if(is.character, as.factor)

test_data <- test_data %>%
  mutate_if(is.character, as.factor)


# Fit tree
tree_model <- tree(Purchase ~ ., data = train_data)

# Print summary
summary(tree_model)

# Calculate training error rate
train_pred <- predict(tree_model, train_data, type = "class")
train_error <- mean(train_pred != train_data$Purchase)

cat("Training Error Rate:", train_error, "\n")
cat("Number of Terminal Nodes:", length(unique(tree_model$where)), "\n")


# c) Type in the name of the tree object in order to get a detailed text output. Pick one of
# the terminal nodes, and interpret the information displayed.

# Print detailed text output
print(tree_model)


# d) Create a plot of the tree, and interpret the results.
# Plot tree
plot(tree_model)
text(tree_model, pretty = 0)
title("Classification Tree for Purchase")

# e) Predict the response on the test data, and produce a confusion matrix comparing the
# test labels to the predicted test labels. What is the test error rate?

# Make predictions on test set
test_pred <- predict(tree_model, test_data, type = "class")

# Create confusion matrix
conf_matrix <- table(Actual = test_data$Purchase, Predicted = test_pred)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate test error rate
test_error <- mean(test_pred != test_data$Purchase)
cat("\nTest Error Rate:", test_error)


# f) Apply the cv.tree() function to the training set in order to determine the optimal tree size.
# Perform cross-validation
cv_tree <- cv.tree(tree_model, FUN = prune.misclass)

# Print results
print(cv_tree)

# g) Produce a plot with tree size on the x-axis and cross-validated classification error rate on the y-axis.
# Create plot
plot(cv_tree$size, cv_tree$dev, type = "b",
     xlab = "Tree Size", 
     ylab = "Cross-Validation Error Rate",
     main = "Cross-Validation Error vs Tree Size")
lines(cv_tree$size, cv_tree$dev)


## Problem 2

library(gbm)
library(randomForest)
library(class)
library(caret)

# Load data
caravan_data <- read.csv('CARAVAN.csv')

str(caravan_data)

# Check the structure and unique values in Purchase
str(caravan_data$Purchase)
unique(caravan_data$Purchase)

caravan_data$Purchase <- ifelse(caravan_data$Purchase == "Yes", 1, 0)

str(caravan_data$Purchase)

# a) Create a training set consisting of the first 1,000 observations, and a test set consisting
# of the remaining observations.

# Split data
train_data <- caravan_data[1:1000, ]
test_data <- caravan_data[1001:nrow(caravan_data), ]

# Remove zero-variance predictors
nzv <- nearZeroVar(train_data)
if(length(nzv) > 0) {
  train_data <- train_data[, -nzv]
  test_data <- test_data[, -nzv]
}

# Scale the data
preProc <- preProcess(train_data[, -which(names(train_data) == "Purchase")], 
                      method = c("center", "scale"))
train_scaled <- predict(preProc, train_data)
test_scaled <- predict(preProc, test_data)


# b) Fit a boosting model to the training set with Purchase as the response and the other
# variables as predictors. Use 1,000 trees, and a shrinkage value of 0.01. Which predictors
# appear to be the most important?

# Fit boosting model
boost_model <- gbm(Purchase ~ ., 
                   data = train_scaled,
                   distribution = "bernoulli",
                   n.trees = 1000,
                   shrinkage = 0.01,
                   interaction.depth = 4)

# Show variable importance
summary(boost_model)


# c) Use the boosting model to predict the response on the test data. Predict that a person
# will make a purchase if the estimated probability of purchase is greater than 20 %. Form
# a confusion matrix. What fraction of the people predicted to make a purchase do in fact
# make one? How does this compare with the results obtained from applying random
# forest model, KNN or logistic regression to this data set?

# Boosting predictions
boost_probs <- predict(boost_model, test_scaled, n.trees = 1000, type = "response")
boost_pred <- ifelse(boost_probs > 0.2, "Yes", "No")

# Random Forest
rf_model <- randomForest(factor(Purchase) ~ ., data = train_scaled)
rf_probs <- predict(rf_model, test_scaled, type = "prob")[,2]
rf_pred <- ifelse(rf_probs > 0.2, "Yes", "No")

# KNN
knn_pred <- knn(train = train_scaled[, -which(names(train_scaled) == "Purchase")],
                test = test_scaled[, -which(names(test_scaled) == "Purchase")],
                cl = train_scaled$Purchase,
                k = 3,
                prob = TRUE)

# Logistic Regression
glm_model <- glm(Purchase ~ ., data = train_scaled, family = "binomial")
glm_probs <- predict(glm_model, test_scaled, type = "response")
glm_pred <- ifelse(glm_probs > 0.2, "Yes", "No")

# Calculate precision for each model
get_precision <- function(pred, actual) {
  conf_matrix <- table(Predicted = pred, Actual = actual)
  return(conf_matrix[2,2] / sum(conf_matrix[2,]))
}

cat("Boosting Precision:", get_precision(boost_pred, test_data$Purchase), "\n")
cat("Random Forest Precision:", get_precision(rf_pred, test_data$Purchase), "\n")
cat("KNN Precision:", get_precision(knn_pred, test_data$Purchase), "\n")
cat("Logistic Regression Precision:", get_precision(glm_pred, test_data$Purchase), "\n")



## Problem 3

library(stats)
library(ggplot2)


# Generate a simulated data set with 20 observations in each of three classes (i.e. 60
# observations total), and 50 variables. Hint: There are a number of functions in R that you
# can use to generate data. One example is the rnorm() function; runif() is another option.
# Be sure to add a mean shift to the observations in each class so that there are three
# distinct classes.


set.seed(123)

# Generate data for three classes
generate_class_data <- function(n_obs, n_vars, mean_shift) {
  matrix(rnorm(n_obs * n_vars, mean = mean_shift, sd = 1), nrow = n_obs)
}

class1 <- generate_class_data(20, 50, 0)
class2 <- generate_class_data(20, 50, 5)
class3 <- generate_class_data(20, 50, 10)

# Combine data
sim_data <- rbind(class1, class2, class3)
true_labels <- factor(rep(1:3, each = 20))


# b) Perform PCA on the 60 observations and plot the first two principal component score
# vectors. Use a different color to indicate the observations in each of the three classes. If
# the three classes appear separated in this plot, then continue on to part (c). If not, then
# return to part (a) and modify the simulation so that there is greater separation between
# the three classes. Do not continue to part (c) until the three classes show at least some
# separation in the first two principal component score vectors.

# Perform PCA
pca_result <- prcomp(sim_data, scale. = TRUE)
pca_scores <- as.data.frame(pca_result$x[, 1:2])
pca_scores$class <- true_labels

# Plot
ggplot(pca_scores, aes(x = PC1, y = PC2, color = class)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "First Two Principal Components",
       x = "First Principal Component",
       y = "Second Principal Component")


# c) Perform K-means clustering of the observations with K = 3. How well do the clusters that
# you obtained in K-means clustering compare to the true class labels? Hint: You can use
# the table() function in R to compare the true class labels to the class labels obtained by
# clustering. Be careful how you interpret the results: K-means clustering will arbitrarily
# number the clusters, so you cannot simply check whether the true class labels and
# clustering labels are the same.

# Perform K-means clustering
km3 <- kmeans(sim_data, centers = 3, nstart = 20)

# Compare with true labels
table(true_labels, km3$cluster)


# d) Perform K-means clustering with K = 2. Describe your results.
km2 <- kmeans(sim_data, centers = 2, nstart = 20)
table(true_labels, km2$cluster)

# e) Now perform K-means clustering with K = 4, and describe your results.
km4 <- kmeans(sim_data, centers = 4, nstart = 20)
table(true_labels, km4$cluster)

# f) Now perform K-means clustering with K = 3 on the first two principal component score
# vectors, rather than on the raw data. That is, perform K-means clustering on the 60 × 2
# matrix of which the first column is the first principal component score vector, and the
# second column is the second principal component score vector. Comment on the
# results.

# K-means on first two PCs
km3_pca <- kmeans(pca_scores[, 1:2], centers = 3, nstart = 20)
table(true_labels, km3_pca$cluster)

# g) Using the scale() function, perform K-means clustering with K = 3 on the data after
# scaling each variable to have standard deviation one. How do these results compare to
# those obtained in (b)? Explain.

# Scale data
scaled_data <- scale(sim_data)

# K-means on scaled data
km3_scaled <- kmeans(scaled_data, centers = 3, nstart = 20)
table(true_labels, km3_scaled$cluster)

sessionInfo()