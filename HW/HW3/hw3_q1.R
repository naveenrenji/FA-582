# Load necessary libraries
library(ISLR)
library(ggplot2)
library(dplyr)
library(corrplot)
library(MASS)

weekly_data <- read.csv("Weekly.csv")


# Numerical and graphical data analysis 
summary(weekly_data)

# Descriptive statistics for continuous variables
means <- sapply(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], mean, na.rm = TRUE)
medians <- sapply(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], median, na.rm = TRUE)
modes <- sapply(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
})
sds <- sapply(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], sd, na.rm = TRUE)
variances <- sapply(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], var, na.rm = TRUE)
skewness <- sapply(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], function(x) {
  library(e1071)
  skewness(x, na.rm = TRUE)
})
kurtoses <- sapply(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], function(x) {
  library(e1071)
  kurtosis(x, na.rm = TRUE)
})

# Combine all descriptive statistics
desc_stats <- data.frame(means, medians, modes, sds, variances, skewness, kurtoses)

# Correlation analysis
correlations <- cor(Weekly[, c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")], use = "complete.obs")

# Time series descriptive statistics
autocorrelation_today <- acf(Weekly$Today, type = "correlation", plot = FALSE)$acf[2] # lag 1 autocorrelation

# Tabulation and frequency counts for the categorical variable 'Direction'
direction_counts <- table(Weekly$Direction)

# Output the results
print(desc_stats)
print(correlations)
print(autocorrelation_today)
print(direction_counts)

#time series data anylsis
# Aggregate the data by year to get average 'Today' values
yearly_returns <- aggregate(Today ~ Year, data = Weekly, mean)

# Plot average returns over time by year
ggplot(yearly_returns, aes(x = Year, y = Today)) +
  geom_line() +
  labs(title = "Average Weekly Returns by Year", x = "Year", y = "Average Return")

# Plot total volume over time by year
yearly_volume <- aggregate(Volume ~ Year, data = Weekly, sum)
ggplot(yearly_volume, aes(x = Year, y = Volume)) +
  geom_line() +
  labs(title = "Total Annual Trading Volume", x = "Year", y = "Total Volume")


# 2. Log regression with direction 

Weekly$Direction <- as.factor(Weekly$Direction)

# Perform logistic regression
logistic_model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family=binomial)
summary(logistic_model)

# 3. confusion matrix and overall fraction of correct predictions.

# Make predictions on the dataset
fitted_probabilities <- predict(logistic_model, type = "response")
predicted_direction <- ifelse(fitted_probabilities > 0.5, "Up", "Down")

# Convert to factor using the same levels as the actual 'Direction' variable
predicted_direction <- factor(predicted_direction, levels = levels(Weekly$Direction))

# Create the confusion matrix
confusion_matrix <- table(Predicted = predicted_direction, Actual = Weekly$Direction)
print(confusion_matrix)

# Calculate the overall fraction of correct predictions
overall_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

print(overall_accuracy)


#logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor

# Divide the data into training and test sets
train_set <- Weekly[Weekly$Year <= 2008, ]
test_set <- Weekly[Weekly$Year > 2008, ]

# Fit logistic regression model using the training set
train_model <- glm(Direction ~ Lag2, data=train_set, family=binomial)

# Predict using the test set
test_probabilities <- predict(train_model, newdata=test_set, type="response")
test_predictions <- ifelse(test_probabilities > 0.5, "Up", "Down")

# Convert predictions to a factor with levels matching the actual data
test_predictions <- factor(test_predictions, levels=levels(test_set$Direction))

# Create the confusion matrix for the test set
confusion_matrix_test <- table(Predicted = test_predictions, Actual = test_set$Direction)
print(confusion_matrix_test)

# Calculate the overall fraction of correct predictions for the test set
overall_accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)

# Print the overall accuracy for the test set
print(overall_accuracy_test)

# 5. LDA

# Divide the data into training and test sets
train_set <- Weekly[Weekly$Year <= 2008, ]
test_set <- Weekly[Weekly$Year > 2008, ]

# Fit LDA model using the training set
lda_model <- lda(Direction ~ Lag2, data = train_set)

# Predict using the test set
lda_predictions <- predict(lda_model, newdata = test_set)
lda_confusion_matrix <- table(Predicted = lda_predictions$class, Actual = test_set$Direction)
lda_confusion_matrix
# Calculate the overall fraction of correct predictions for LDA on the test set
lda_accuracy <- sum(diag(lda_confusion_matrix)) / sum(lda_confusion_matrix)



# 6. QDA


# Fit QDA model using the training set
qda_model <- qda(Direction ~ Lag2, data = train_set)

# Predict using the test set
qda_predictions <- predict(qda_model, newdata = test_set)
qda_confusion_matrix <- table(Predicted = qda_predictions$class, Actual = test_set$Direction)
qda_confusion_matrix
# Calculate the overall fraction of correct predictions for QDA on the test set
qda_accuracy <- sum(diag(qda_confusion_matrix)) / sum(qda_confusion_matrix)


# 7. KNN

# Prepare the predictors and response for KNN
train_predictor <- as.matrix(train_set$Lag2)
test_predictor <- as.matrix(test_set$Lag2)
train_response <- train_set$Direction

# Fit KNN model using the training set with k = 1
knn_predictions <- knn(train = train_predictor, test = test_predictor, cl = train_response, k = 1)

# Create the confusion matrix for KNN on the test set
knn_confusion_matrix <- table(Predicted = knn_predictions, Actual = test_set$Direction)
knn_confusion_matrix
# Calculate the overall fraction of correct predictions for KNN on the test set
knn_accuracy <- sum(diag(knn_confusion_matrix)) / sum(knn_confusion_matrix)


# 8. the best model - 
lda_accuracy
qda_accuracy
knn_accuracy


# 9. Experimenting 

# Define the training and test sets
train_set <- Weekly[Weekly$Year <= 2008, ]
test_set <- Weekly[Weekly$Year > 2008, ]

# This function will be used to calculate the accuracy from the confusion matrix
calculate_accuracy <- function(confusion_matrix) {
  sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

# Initialize a data frame to store the results
results <- data.frame(Method = character(), Variables = character(), Accuracy = numeric())

# LDA with different combinations of predictors
lda_formulas <- c(Direction ~ Lag2,
                  Direction ~ Lag2 + Lag1,
                  Direction ~ Lag2 * Volume)  # Example of including interaction

for (formula in lda_formulas) {
  lda_model <- lda(formula, data = train_set)
  lda_predictions <- predict(lda_model, newdata = test_set)$class
  lda_confusion_matrix <- table(Predicted = lda_predictions, Actual = test_set$Direction)
  print(formula)
  print(lda_confusion_matrix)
  lda_accuracy <- calculate_accuracy(lda_confusion_matrix)
  results <- rbind(results, data.frame(Method = "LDA", Variables = deparse(formula), Accuracy = lda_accuracy))
}

# QDA with different combinations of predictors
qda_formulas <- c(Direction ~ Lag2,
                  Direction ~ Lag2 + Lag1,
                  Direction ~ Lag2 * Volume)  # Example of including interaction

for (formula in qda_formulas) {
  qda_model <- qda(formula, data = train_set)
  qda_predictions <- predict(qda_model, newdata = test_set)$class
  qda_confusion_matrix <- table(Predicted = qda_predictions, Actual = test_set$Direction)
  qda_accuracy <- calculate_accuracy(qda_confusion_matrix)
  results <- rbind(results, data.frame(Method = "QDA", Variables = deparse(formula), Accuracy = qda_accuracy))
}

# KNN with different values of K and predictors
k_values <- 1:20  # Example range of K values to try
knn_formulas <- c(~ Lag2,
                  ~ Lag2 + Lag1,
                  ~ Lag2 * Volume)  # Example of including interaction

for (k in k_values) {
  for (formula in knn_formulas) {
    train_predictor <- as.data.frame(model.matrix(formula, data = train_set)[,-1])
    test_predictor <- as.data.frame(model.matrix(formula, data = test_set)[,-1])
    knn_predictions <- knn(train = train_predictor, test = test_predictor, cl = train_set$Direction, k = k)
    knn_confusion_matrix <- table(Predicted = knn_predictions, Actual = test_set$Direction)
    knn_accuracy <- calculate_accuracy(knn_confusion_matrix)
    results <- rbind(results, data.frame(Method = paste("KNN", k), Variables = deparse(formula), Accuracy = knn_accuracy))
  }
}
results


# Sort results to find the best model
best_model <- results[which.max(results$Accuracy), ]
print(best_model)
