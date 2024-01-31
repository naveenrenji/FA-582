library(gbm)

# Load the dataset
caravan_data <- read.csv("CARAVAN.csv")

str(caravan_data)

# Convert 'Purchase' from character to factor and then to binary numeric
caravan_data$Purchase <- as.numeric(as.factor(caravan_data$Purchase)) - 1

# Check for variables with no variation and remove them
no_variation <- sapply(caravan_data, function(x) length(unique(x)) == 1)
caravan_data <- caravan_data[, !no_variation]

#-------------------------------------- a -------------------------------------------------------#

# Create the training set
train_set <- caravan_data[1:1000, ]

# Create the test set
test_set <- caravan_data[1001:nrow(caravan_data), ]

#-------------------------------------- b -------------------------------------------------------#

# Fit the boosting model
set.seed(123) # for reproducibility
boosting_model <- gbm(Purchase ~ ., 
                      distribution = "bernoulli", 
                      data = train_set, 
                      n.trees = 1000, 
                      interaction.depth = 1, 
                      shrinkage = 0.01,
                      n.minobsinnode = 10)

# Summary of the boosting model to see most important predictors
summary(boosting_model)


#-------------------------------------- c -------------------------------------------------------#

# Predicting on the test set using the boosting model
test_pred_prob <- predict(boosting_model, newdata = test_set, n.trees = 1000, type = "response")

# Predicting a purchase if the estimated probability is greater than 20%
test_pred_class <- ifelse(test_pred_prob > 0.20, 1, 0)

# Creating a confusion matrix
conf_matrix <- table(test_set$Purchase, test_pred_class)
print(conf_matrix)

# Calculating the fraction of correct predictions among those predicted to make a purchase
purchase_predicted_correctly <- conf_matrix[2, 2]
purchase_predicted_total <- sum(conf_matrix[, 2])
fraction_correct <- purchase_predicted_correctly / purchase_predicted_total
print(fraction_correct)


# Fitting logistic regression model
logistic_model <- glm(Purchase ~ ., family = binomial, data = train_set)

# Predicting on the test set using the logistic regression model
logistic_pred_prob <- predict(logistic_model, newdata = test_set, type = "response")
logistic_pred_class <- ifelse(logistic_pred_prob > 0.20, 1, 0)  # Using the same 20% threshold

# Creating a confusion matrix for logistic regression
conf_matrix_logistic <- table(test_set$Purchase, logistic_pred_class)
print(conf_matrix_logistic)

# Calculating the fraction of correct predictions in log reg
purchase_predicted_correctly_log <- conf_matrix_logistic[2, 2]
purchase_predicted_total_log <- sum(conf_matrix_logistic[, 2])
fraction_correct_log <- purchase_predicted_correctly_log / purchase_predicted_total_log
print(fraction_correct_log)

