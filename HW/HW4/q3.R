#-------------------------------------- a -------------------------------------------------------#

# Set seed for reproducibility
set.seed(123)

# Number of observations per class and total variables
n <- 20
p <- 50

# Generating data for three classes
class_1 <- matrix(rnorm(n * p, mean = 0), nrow = n)
class_2 <- matrix(rnorm(n * p, mean = 1), nrow = n)
class_3 <- matrix(rnorm(n * p, mean = 2), nrow = n)

# Combining the classes into one data set
data <- rbind(class_1, class_2, class_3)

str(data)

#-------------------------------------- b -------------------------------------------------------#

# Perform PCA
pca_result <- prcomp(data, scale. = TRUE)

# Create a data frame with the PCA scores and class labels
pca_scores <- data.frame(pca_result$x[,1:2])
pca_scores$class <- rep(1:3, each = 20)

# Plotting the first two principal components
library(ggplot2)
ggplot(pca_scores, aes(x = PC1, y = PC2, color = factor(class))) +
  geom_point() +
  ggtitle("PCA of Simulated Data") +
  theme_minimal() +
  labs(color = "Class")


#-------------------------------------- c -------------------------------------------------------#

# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(data, centers = 3)

# Compare K-means clusters to true class labels
comparison_table <- table(TrueLabels = pca_scores$class, Cluster = kmeans_result$cluster)
print(comparison_table)

#-------------------------------------- d -------------------------------------------------------#


set.seed(123) # Set a random seed for reproducibility
kmeans_result_k2 <- kmeans(data, centers = 2)
# Print the resulting clusters
print(kmeans_result_k2$cluster)

# Compare K-means clusters to true class labels
comparison_table_k2 <- table(TrueLabels = pca_scores$class, Cluster = kmeans_result_k2$cluster)
print(comparison_table_k2)


#-------------------------------------- e -------------------------------------------------------#

set.seed(123) # Set a random seed for reproducibility
kmeans_result_k4 <- kmeans(data, centers = 4)
# Print the resulting clusters
print(kmeans_result_k4$cluster)

# Compare K-means clusters to true class labels
comparison_table_k4 <- table(TrueLabels = pca_scores$class, Cluster = kmeans_result_k4$cluster)
print(comparison_table_k4)


#-------------------------------------- f -------------------------------------------------------#


# Extracting the first two principal components
pca_scores_2d <- pca_result$x[, 1:2]

# Perform K-means clustering on the PCA scores
set.seed(123) # Set a random seed for reproducibility
kmeans_result_pca <- kmeans(pca_scores_2d, centers = 3)

# Print the resulting clusters
print(kmeans_result_pca$cluster)

# Compare K-means clusters to true class labels
comparison_table_pca <- table(TrueLabels = pca_scores$class, Cluster = kmeans_result_pca$cluster)
print(comparison_table_pca)


#-------------------------------------- g -------------------------------------------------------#


# Scaling the data
data_scaled <- scale(data)

# Perform K-means clustering on the scaled data
set.seed(123) # Set a random seed for reproducibility
kmeans_result_scaled <- kmeans(data_scaled, centers = 3)

# Print the resulting clusters
print(kmeans_result_scaled$cluster)

# Compare K-means clusters to true class labels
comparison_table_scaled <- table(TrueLabels = rep(1:3, each = 20), Cluster = kmeans_result_scaled$cluster)
print(comparison_table_scaled)

