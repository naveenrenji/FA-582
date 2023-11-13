# Load necessary libraries
library(ISLR)
library(ggplot2)
library(dplyr)
library(corrplot)
library(MASS)
library(GGally)
library(caret)
library(class)

# Load the data
auto <- read.csv("Auto.csv")

summary(auto) 

# 1. MPG01 

# Calculate the median of the mpg column
median_mpg <- median(auto$mpg)

# Create the binary variable mpg01 where 1 if mpg is above its median and 0 if below
auto$mpg01 <- ifelse(auto$mpg > median_mpg, 1, 0)
auto_complete <- auto
str(auto_complete)
summary(auto_complete) 

# 2. Most associated feature 

# We will use only the numeric variables for the pair plot excluding 'mpg' as it is continuous
numeric_vars <- names(auto_complete)[sapply(auto_complete, is.numeric)]
numeric_vars <- setdiff(numeric_vars, "mpg") # exclude 'mpg'

# Create the pair plot
ggpairs(auto_complete[, c("mpg01", numeric_vars)], aes(color = as.factor(mpg01)), 
        lower = list(continuous = wrap("points", size = 1.5, alpha = 0.5)),
        diag = list(continuous = wrap("barDiag", fill = "darkgray")),
        upper = list(continuous = wrap("cor", size = 3)))


# containing the names of the variables most associated with mpg01 as determined from above
most_associated_vars <- c("weight", "displacement", "horsepower", "cylinders")

# Split the data into training and test sets
set.seed(123)  # for reproducibility
index <- createDataPartition(auto_complete$mpg01, p = 0.75, list = FALSE)
training <- auto_complete[index, ]
test <- auto_complete[-index, ]

# LDA
lda_fit <- lda(mpg01 ~ ., data = training[, c("mpg01", most_associated_vars)])
lda_pred <- predict(lda_fit, test[, most_associated_vars])
lda_confusion <- table(Predicted = lda_pred$class, Actual = test$mpg01)
lda_accuracy <- sum(diag(lda_confusion)) / sum(lda_confusion)
lda_test_error <- mean(lda_pred$class != test$mpg01)

# QDA
qda_fit <- qda(mpg01 ~ ., data = training[, c("mpg01", most_associated_vars)])
qda_pred <- predict(qda_fit, test[, most_associated_vars])
qda_test_error <- mean(qda_pred$class != test$mpg01)
qda_confusion <- table(Predicted = qda_pred$class, Actual = test$mpg01)
qda_accuracy <- sum(diag(qda_confusion)) / sum(qda_confusion)

# Logistic Regression
logit_fit <- glm(mpg01 ~ ., data = training[, c("mpg01", most_associated_vars)], family = binomial)
logit_pred <- predict(logit_fit, test[, most_associated_vars], type = "response")
logit_test_error <- mean(ifelse(logit_pred > 0.5, 1, 0) != test$mpg01)
logit_pred_class <- ifelse(logit_pred > 0.5, 1, 0)
logit_confusion <- table(Predicted = logit_pred_class, Actual = test$mpg01)
logit_accuracy <- sum(diag(logit_confusion)) / sum(logit_confusion)


# KNN
set.seed(123) # for reproducibility
k_values <- c(1, 3, 5, 7, 9, 11, 13, 15) # set of k values
knn_test_errors <- sapply(k_values, function(k) {
  knn_pred <- knn(train = training[, most_associated_vars], test = test[, most_associated_vars],
                  cl = training$mpg01, k = k)
  mean(knn_pred != test$mpg01)
})


# Finding Which K is best?
best_k <- k_values[which.min(knn_test_errors)]
best_k_error <- min(knn_test_errors)

# Doing accuracy and confusion with the best-k 
set.seed(123) # for reproducibility
knn_pred <- knn(train = training[, most_associated_vars], test = test[, most_associated_vars],
                cl = training$mpg01, k = best_k)
knn_confusion <- table(Predicted = knn_pred, Actual = test$mpg01)
knn_accuracy <- sum(diag(knn_confusion)) / sum(knn_confusion)

#outputting the accuracy and confusioon matrix for each
list(
  lda_accuracy = lda_accuracy,
  lda_confusion = lda_confusion,
  qda_accuracy = qda_accuracy,
  qda_confusion = qda_confusion,
  logit_accuracy = logit_accuracy,
  logit_confusion = logit_confusion,
  knn_accuracy = knn_accuracy,
  knn_confusion = knn_confusion
)

# Output the errors for each method
list(lda_test_error = lda_test_error, qda_test_error = qda_test_error, 
     logit_test_error = logit_test_error, best_k = best_k, best_k_error = best_k_error)
