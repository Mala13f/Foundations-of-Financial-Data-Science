## Foundations of financial data science - FA582
## Assignment 3
## Federica Malamisura

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(stringr)
library(MASS)
library(ISLR)
library(class)

# Set the working directory
setwd("/Users/federicamalamisura/Desktop/Found of Financial Data Science/Assignment 3/HW3_data")
getwd

# Load all the neccesary data 
weekly_data <- read.csv("Weekly.csv")


## Overview of the Weekly Financial Data
# The weekly_data CSV contains financial information spanning from 1990 to 2010, featuring various variables.

# Year: This column indicates the year of each observation.
# Lag1 to Lag5: These columns represent the returns from the previous five weeks, with Lag1 corresponding to one week ago, Lag2 to two weeks ago, and so forth.
# Volume: This measures the number of shares traded in billions during that week, providing insight into market activity.
# Today: This column shows the percentage return for the current week.
# Direction: A categorical variable that indicates whether the market moved "Up" (positive return) or "Down" (negative return) for that week.

# This dataset is useful for analyzing the relationship between past returns and trading volume in relation to the current week's market performance.


# PROBLEM 1

# a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?

head(weekly_data)
tail(weekly_data)

summary(weekly_data)

# Correlation matrix (useful for understanding relationships)
cor(weekly_data[,-9])  # Exclude the 'Direction' column (categorical)
print(cor(weekly_data[,-9]) )

# Graphical summary
pairs(weekly_data[,-9])  # Scatterplot matrix to explore relationships


# b) Use the full data set to perform a logistic regression with Direction as the response and
# the five lag variables plus Volume as predictors. Use the summary function to print the
# results. Do any of the predictors appear to be statistically significant? If so, which ones?

#First we need to encode the binary bariable, so "Up" will be 1 and "Down" will be 0. 
weekly_data$DirectionBinary <- ifelse(weekly_data$Direction == "Up", 1, 0)

# Where direction is the stationary variables
log_model <- glm(DirectionBinary ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=weekly_data, family=binomial)

# Model summary
summary(log_model)


# c) Compute the confusion matrix and overall fraction of correct predictions. Explain what
# the confusion matrix is telling you about the types of mistakes made by logistic regression.

# Predicted probabilities
pred_probs <- predict(log_model, type="response")

# Convert probabilities to binary predictions (Up if prob > 0.5)
pred_direction <- ifelse(pred_probs > 0.5, "Up", "Down")

# Confusion matrix
table(predicted=pred_direction, actual=weekly_data$Direction)

# Calculate accuracy
accuracy_1 <- mean(pred_direction == weekly_data$Direction)



# d) Now fit the logistic regression model using a training data period from 1990 to 2008,
# with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction
# of correct predictions for the held out data (that is, the data from 2009 and 2010).

# Split data in training and tetsing sets
train_data <- subset(weekly_data, Year <= 2008)
test_data <- subset(weekly_data, Year > 2008)

# Fit the logistic regression using Lag2 as the predictor 
train_log_model <- glm(DirectionBinary ~ Lag2, data=train_data, family=binomial)

# Predict the probabilities on the test 
test_pred_probs <- predict(train_log_model, test_data, type="response")

# Convert probabilities to binary predictions (Up if prob > 0.5, Down otherwise)
#test_pred_direction<- ifelse(test_pred_probs > 0.5, 1, 0)
test_actual_direction <- ifelse(test_data$DirectionBinary == 1, "Up", "Down")
test_pred_direction<- ifelse(test_pred_probs > 0.5, "Up", "Down")

# Confusion matrix
conf_matrix <- table(predicted = test_pred_direction, actual=test_actual_direction)

# Calculate accuracy 
# Calculate the overall fraction of correct predictions (accuracy)
accuracy <- mean(test_pred_direction == test_actual_direction)
print(accuracy)


# e) Repeat d) using LDA.

# Fit the LDA model using Lag2 as the only predictor on the training set (1990-2008)
train_lda_model <- lda(DirectionBinary ~ Lag2, data=train_data)

# Predict probabilities on the test set
lda_pred <- predict(train_lda_model, test_data)

# To better understand, convert predicted class values to "Up" and "Down"
test_pred_direction_lda <- ifelse(lda_pred$class == 1, "Up", "Down")

# Confusion matrix for LDA
lda_conf_matrix <- table(predicted = test_pred_direction_lda, actual = test_actual_direction)
print(lda_conf_matrix)

# Calculate accuracy
accuracy_lda <- mean(test_pred_direction_lda == test_actual_direction)
print(accuracy_lda)


# f) Repeat d) using QDA

# Fit the QDA model using Lag2 as the only predictor on the training set (1990-2008)
train_qda_model <- qda(DirectionBinary ~ Lag2, data=train_data)

# Predict the class probabilities on the test set (2009-2010)
qda_pred <- predict(train_qda_model, test_data)

# Convert predicted class values to "Up" and "Down"
test_pred_direction_qda <- ifelse(qda_pred$class == 1, "Up", "Down")

# Confusion matrix for QDA
qda_conf_matrix <- table(predicted = test_pred_direction_qda, actual = test_actual_direction)
print(qda_conf_matrix)

# Calculate accuracy for QDA
accuracy_qda <- mean(test_pred_direction_qda == test_actual_direction)
print(accuracy_qda)

# g) KNN for N = 1

# Extract the Lag2 predictor (in both sets) for KNN
train_X <- as.matrix(train_data$Lag2)  
test_X <- as.matrix(test_data$Lag2)    

# Select the response variable in each set, DirectionBinary
train_Y <- train_data$DirectionBinary  
test_Y <- test_data$DirectionBinary    

# Apply KNN with K = 1, whcih is a fucntion in our imported package
knn_pred <- knn(train_X, test_X, train_Y, k = 1)

# Convert predictions to "Up" and "Down"
test_pred_direction_knn <- ifelse(knn_pred == 1, "Up", "Down")
test_actual_direction <- ifelse(test_Y == 1, "Up", "Down")

# Confusion matrix for KNN
conf_matrix_knn <- table(predicted = test_pred_direction_knn, actual = test_actual_direction)
print(conf_matrix_knn)

# Calculate accuracy for KNN
accuracy_knn <- mean(test_pred_direction_knn == test_actual_direction)
print(accuracy_knn)

# i) Experiment with different combinations of predictors, including possible
# transformations and interactions, for each of the methods. Report the variables,
# method, and associated confusion matrix that appears to provide the best results on the
# held out data. Note that you should also experiment with values for K in the KNN
# classifier.

test_actual_direction <- ifelse(test_data$DirectionBinary == 1, "Up", "Down")

# Logistic regression, using Lag1, Lag2, Volume and interaction.  
log_model_1 <- glm(DirectionBinary ~ Lag1 + Lag2 + Volume + Lag1:Lag2, data=train_data, family=binomial)

# Predict the probabilities on the test set
test_pred_probs <- predict(log_model_1, test_data, type="response")

# Convert probabilities to binary predictions (Up if prob > 0.5, Down otherwise)
test_pred_direction <- ifelse(test_pred_probs > 0.5, "Up", "Down")

# Confusion matrix for logistic regression
conf_matrix_log <- table(predicted = test_pred_direction, actual = test_actual_direction)
print(conf_matrix_log)

# Calculate accuracy for logistic regression
accuracy_log <- mean(test_pred_direction == test_actual_direction)
print(accuracy_log)

# higher order interaction terms 

log_model_2 <- glm(DirectionBinary ~ Lag1 + Lag2 + Volume + Lag1:Lag2 + I(Lag1^2) + I(Lag2^2), data=train_data, family=binomial)

# Predict the probabilities on the test set
test_pred_probs_2 <- predict(log_model_2, test_data, type="response")

# Convert probabilities to binary predictions (Up if prob > 0.5, Down otherwise)
test_pred_direction_2 <- ifelse(test_pred_probs_2 > 0.5, "Up", "Down")

# Confusion matrix for logistic regression
conf_matrix_log_2 <- table(predicted = test_pred_direction_2, actual = test_actual_direction)
print(conf_matrix_log_2)

# Calculate accuracy for logistic regression
accuracy_log_2 <- mean(test_pred_direction_2 == test_actual_direction)
print(accuracy_log_2)


# Lets try non-linear transformations

log_model_3 <- glm(DirectionBinary ~ Lag1 + Lag2 + Volume + Lag1:Lag2 + I(Lag1^2) + I(Lag2^2), data=train_data, family=binomial)

# Predict the probabilities on the test set
test_pred_probs_3 <- predict(log_model_3, test_data, type="response")

# Convert probabilities to binary predictions (Up if prob > 0.5, Down otherwise)
test_pred_direction_3 <- ifelse(test_pred_probs_3 > 0.5, "Up", "Down")

# Confusion matrix for logistic regression
conf_matrix_log_3 <- table(predicted = test_pred_direction_3, actual = test_actual_direction)
print(conf_matrix_log_3)

# Calculate accuracy for logistic regression
accuracy_log_3 <- mean(test_pred_direction_3 == test_actual_direction)
print(accuracy_log_3)


# LDA using other lags

# LDA: Using Lag1, Lag2, and Volume
lda_model <- lda(DirectionBinary ~ Lag1 + Lag2 + Volume + Lag1:Lag2, data=train_data)

# Predict the classes for the test set
lda_pred <- predict(lda_model, test_data)
test_pred_direction_lda <- ifelse(lda_pred$class == 1, "Up", "Down")

# Confusion matrix for LDA
conf_matrix_lda <- table(predicted = test_pred_direction_lda, actual = test_actual_direction)
print(conf_matrix_lda)

# Calculate accuracy for LDA
accuracy_lda <- mean(test_pred_direction_lda == test_actual_direction)
print(accuracy_lda)

# QDA,  Using Lag1, Lag2, and their interaction

qda_model <- qda(DirectionBinary ~ Lag1 + Lag2 + Volume + Lag1:Lag2, data=train_data)

# Predict the classes for the test set
qda_pred <- predict(qda_model, test_data)
test_pred_direction_qda <- ifelse(qda_pred$class == 1, "Up", "Down")

# Confusion matrix for QDA
conf_matrix_qda <- table(predicted = test_pred_direction_qda, actual = test_actual_direction)
print(conf_matrix_qda)

# Calculate accuracy for QDA
accuracy_qda <- mean(test_pred_direction_qda == test_actual_direction)
print(accuracy_qda)

# KNN for K = 3,5,7

# Prepare the predictors for KNN
train_X <- as.matrix(train_data[, c("Lag1", "Lag2", "Volume")])
test_X <- as.matrix(test_data[, c("Lag1", "Lag2", "Volume")])

train_Y <- train_data$DirectionBinary

# Experiment with different values for K
for (k in c(3, 5, 7)) {
  knn_pred <- knn(train_X, test_X, train_Y, k = k)
  test_pred_direction_knn <- ifelse(knn_pred == 1, "Up", "Down")
  
  # Confusion matrix for KNN
  conf_matrix_knn <- table(predicted = test_pred_direction_knn, actual = test_actual_direction)
  print(paste("Confusion Matrix for K =", k))
  print(conf_matrix_knn)
  
  # Calculate accuracy for KNN
  accuracy_knn <- mean(test_pred_direction_knn == test_actual_direction)
  print(paste("Accuracy for K =", k, ":"))
  print(accuracy_knn)
}



# PROBLEM 2: 

# Set the working directory
setwd("/Users/federicamalamisura/Desktop/Found of Financial Data Science/Assignment 3/HW3_data")
getwd

# Load all the neccesary data 
auto <- read.csv("Auto.csv")

# a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above its
# median, and a 0 if mpg contains a value below its median. You can compute the median
# using the median() function. Note you may find it helpful to use the data.frame()
# function to create a single data set containing both mpg01 and the other Auto variables.

# Calculate the median of mpg
mpg_median <- median(auto$mpg)

# Create the binary variable mpg01: 1 if mpg is above the median, 0 if below
mpg01 <- ifelse(auto$mpg > mpg_median, 1, 0)

# Combine mpg01 with the original Auto dataset
auto_mpg01 <- data.frame(mpg01, auto)

# Display the first few rows of the new dataset to check
head(auto_mpg01)

# b) Explore the data graphically in order to investigate the association between mpg01 and
# the other features. Which of the other features seem most likely to be useful in 
# predicting mpg01? Scatterplots and boxplots may be useful tools to answer this
# question. Describe your findings.

# Scatterplot: Horsepower vs. mpg01
ggplot(auto_mpg01, aes(x=horsepower, y=mpg01)) +
  geom_point() +
  labs(title="Horsepower vs. mpg01", x="Horsepower", y="mpg01 (High = 1, Low = 0)")

# Scatterplot: Weight vs. mpg01
ggplot(auto_mpg01, aes(x=weight, y=mpg01)) +
  geom_point() +
  labs(title="Weight vs. mpg01", x="Weight", y="mpg01 (High = 1, Low = 0)")

# Scatterplot: Displacement vs. mpg01
ggplot(auto_mpg01, aes(x=displacement, y=mpg01)) +
  geom_point() +
  labs(title="Displacement vs. mpg01", x="Displacement", y="mpg01 (High = 1, Low = 0)")

# Scatterplot: Acceleration vs. mpg01
ggplot(auto_mpg01, aes(x=acceleration, y=mpg01)) +
  geom_point() +
  labs(title="Acceleration vs. mpg01", x="Acceleration", y="mpg01 (High = 1, Low = 0)")

# Create boxplots to compare numerical features between mpg01 = 0 and mpg01 = 1

# Boxplot: Horsepower vs. mpg01
ggplot(auto_mpg01, aes(x=factor(mpg01), y=horsepower)) +
  geom_boxplot() +
  labs(title="Boxplot of Horsepower by mpg01", x="mpg01 (High = 1, Low = 0)", y="Horsepower")

# Boxplot: Weight vs. mpg01
ggplot(auto_mpg01, aes(x=factor(mpg01), y=weight)) +
  geom_boxplot() +
  labs(title="Boxplot of Weight by mpg01", x="mpg01 (High = 1, Low = 0)", y="Weight")

# Boxplot: Displacement vs. mpg01
ggplot(auto_mpg01, aes(x=factor(mpg01), y=displacement)) +
  geom_boxplot() +
  labs(title="Boxplot of Displacement by mpg01", x="mpg01 (High = 1, Low = 0)", y="Displacement")

# Boxplot: Acceleration vs. mpg01
ggplot(auto_mpg01, aes(x=factor(mpg01), y=acceleration)) +
  geom_boxplot() +
  labs(title="Boxplot of Acceleration by mpg01", x="mpg01 (High = 1, Low = 0)", y="Acceleration")


# c) Split the data into a training set and a test set.

set.seed(1) # random seed to ensure reproducibility, sample will remain the same each time we run the code
train_indices <- sample(1:nrow(auto_mpg01), nrow(auto_mpg01) / 2) # value so we select half of the rows
train_data <- auto_mpg01[train_indices, ]
test_data <- auto_mpg01[-train_indices, ]

# d ) Perform LDA on the training data in order to predict mpg01 using the variables that
# seemed most associated with mpg01 in b). What is the test error of the model
# obtained?

# In order to run the LDA, we need to select a number of variables that will work as predictors, so lets us select the 
# most notable variables as observed in b) 

# Select the predictors
predictors <- c("horsepower", "weight")

# Lets locate if there are any NA
na_rows <- train_data[!complete.cases(train_data[, c("horsepower", "weight")]), ]
na_rows

# Perform LDA
lda_model <- lda(mpg01 ~ horsepower + weight, data = train_data)

# Predict on test data
lda_pred <- predict(lda_model, test_data)
lda_class <- lda_pred$class

# Calculate test error
test_error_lda <- mean(lda_class != test_data$mpg01)
test_error_lda

# e) Perform QDA on the training data in order to predict mpg01 using the variables that
# seemed most associated with mpg01 in b). What is the test error of the model
# obtained?

qda_model <- qda(mpg01 ~ horsepower + weight, data = train_data)

# Predict on test data
qda_pred <- predict(qda_model, test_data)
qda_class <- qda_pred$class

# Calculate test error
qda_test_error <- mean(qda_class != test_data$mpg01)
qda_test_error

# f) Perform logistic regression on the training data in order to predict mpg01 using the
# variables that seemed most associated with mpg01 in b). What is the test error of the
# model obtained?

logistic_model <- glm(mpg01 ~ horsepower + weight, data = train_data, family = binomial)

# Predict on test data
logistic_probs <- predict(logistic_model, test_data, type = "response")
logistic_pred <- ifelse(logistic_probs > 0.5, 1, 0)  # Classify based on a 0.5 threshold

# Calculate test error
logistic_test_error <- mean(logistic_pred != test_data$mpg01)
logistic_test_error

# g) Perform KNN on the training data, with several values of K, in order to predict mpg01.
# Use only the variables that seemed most associated with mpg01 in (b). What test errors
# do you obtain? Which value of K seems to perform the best on this data set?

# Prepare training and testing data matrices
train_X <- train_data[, c("horsepower", "weight")]
test_X <- test_data[, c("horsepower", "weight")]
train_y <- train_data$mpg01

# Range of K values to try
k_values <- 1:10
knn_test_errors <- numeric(length(k_values))

# Loop over each K to calculate test error
for (i in k_values) {
  knn_pred <- knn(train_X, test_X, train_y, k = i)
  knn_test_errors[i] <- mean(knn_pred != test_data$mpg01)
}

# Display test errors for each K
knn_test_errors

# Find the K with the lowest test error
best_k <- k_values[which.min(knn_test_errors)]
best_k