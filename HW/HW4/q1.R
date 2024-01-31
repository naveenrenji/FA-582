# Load the dataset
oj_data <- read.csv("OJ.csv")

str(oj_data)

# Convert character variables to factors
oj_data$Purchase <- as.factor(oj_data$Purchase)
oj_data$Store7 <- as.factor(oj_data$Store7)

#-------------------------------------- a -------------------------------------------------------#

# Set seed for reproducibility
set.seed(123)

# Randomly select 800 observations for the training set
train_indices <- sample(1:nrow(oj_data), 800)

# Create the training and test sets
train_set <- oj_data[train_indices, ]
test_set <- oj_data[-train_indices, ]

#-------------------------------------- b -------------------------------------------------------#

# Load the tree package
library(tree)

# Fit the tree model
oj_tree <- tree(Purchase ~ ., data = train_set)

# Summary of the tree
summary(oj_tree)

#-------------------------------------- c -------------------------------------------------------#

print(oj_tree)

#-------------------------------------- d -------------------------------------------------------#

# Plot the tree
plot(oj_tree)

# Add labels to the plot
text(oj_tree, pretty = 0)

#-------------------------------------- e -------------------------------------------------------#

# Predicting on the test set
test_pred <- predict(oj_tree, newdata = test_set, type = "class")

# Creating the confusion matrix
conf_matrix <- table(test_set$Purchase, test_pred)
print(conf_matrix)

# Calculating the test error rate
test_error_rate <- sum(diag(conf_matrix)) / sum(conf_matrix)
test_error_rate <- 1 - test_error_rate
print(test_error_rate)


#-------------------------------------- f -------------------------------------------------------#

# Perform cross-validation
cv_oj_tree <- cv.tree(oj_tree, FUN = prune.tree)

# Print the results
print(cv_oj_tree)

#-------------------------------------- g -------------------------------------------------------#

# Plot tree size vs cross-validated classification error rate
plot(cv_oj_tree$size, cv_oj_tree$dev, type="b",
     xlab="Number of Terminal Nodes", ylab="CV Error",
     main="Plot of Tree Size vs CV Error")



