# Load necessary libraries
library(readr)
library(dplyr)
library(MASS)
library(stats)

# Load the data from the CSV files
fundamentals_df <- read_csv("fundamentals.csv")

quantitative_cols <- c("After Tax ROE", "Cash Ratio", "Current Ratio", "Operating Margin", "Pre-Tax Margin",
                       "Pre-Tax ROE", "Profit Margin", "Quick Ratio", "Total Assets", "Total Liabilities", "Earnings Per Share")

# Filter Data for Specific Year (2013)
fundamentals_2013 <- filter(fundamentals_df, substr(`Period Ending`, 1, 4) == "2013")


fundamentals_2013 <- fundamentals_2013 %>% 
  rowwise() %>% 
  mutate(missing_or_zero = sum(across(all_of(quantitative_cols), ~is.na(.) | . == 0), na.rm = TRUE)) %>% 
  ungroup()

# Sorting the DataFrame based on the missing_or_zero column and then picking the first 100 tickers.
top_100_tickers <- fundamentals_2013 %>% 
  arrange(missing_or_zero) %>% 
  head(100) %>% 
  pull('Ticker Symbol')

fundamentals_2013_subset <- filter(fundamentals_2013, `Ticker Symbol` %in% top_100_tickers)

selected_columns <- c("Ticker Symbol", "After Tax ROE", "Cash Ratio", "Current Ratio", "Operating Margin", "Pre-Tax Margin", "Pre-Tax ROE", "Profit Margin", "Quick Ratio", "Total Assets", "Total Liabilities", "Earnings Per Share")
fundamentals_2013_subset_selected <- dplyr::select(fundamentals_2013_subset, all_of(selected_columns))

# Standardize the data (mean = 0, sd = 1)
# Exclude the 'Ticker Symbol' column when standardizing
fundamentals_2013_subset_selected_std <- fundamentals_2013_subset_selected
fundamentals_2013_subset_selected_std[-1] <- scale(fundamentals_2013_subset_selected[-1])

# Compute the covariance matrix and its inverse
cov_matrix <- cov(fundamentals_2013_subset_selected_std[-1])
inv_cov_matrix <- solve(cov_matrix)

#  Mahalanobis distance
mahalanobis_distance <- function(x, y, inv_cov_matrix) {
  diff <- matrix(x - y, ncol = 1)  # Convert difference to column vector
  dist <- sqrt(t(diff) %*% inv_cov_matrix %*% diff)
  return(dist)
}

# Matrix to store Mahalanobis distances
mahalanobis_matrix <- matrix(NA, 
                             nrow = nrow(fundamentals_2013_subset_selected_std), 
                             ncol = nrow(fundamentals_2013_subset_selected_std))
rownames(mahalanobis_matrix) <- fundamentals_2013_subset_selected_std$`Ticker Symbol`
colnames(mahalanobis_matrix) <- fundamentals_2013_subset_selected_std$`Ticker Symbol`

# Compute Mahalanobis distance for each pair 
for (i in 1:nrow(fundamentals_2013_subset_selected_std)) {
  for (j in 1:nrow(fundamentals_2013_subset_selected_std)) {
    x <- as.numeric(fundamentals_2013_subset_selected_std[i, -1])
    y <- as.numeric(fundamentals_2013_subset_selected_std[j, -1])
    mahalanobis_matrix[i, j] <- mahalanobis_distance(x, y, inv_cov_matrix)
  }
}
mahalanobis_df <- as.data.frame(as.table(mahalanobis_matrix))

sorted_mahalanobis <- mahalanobis_df %>% arrange(Freq)
top_10_mahalanobis <- head(sorted_mahalanobis, 10)
bottom_10_mahalanobis <- tail(sorted_mahalanobis, 10)

print("Top 10 Mahalanobis distances:")
print(top_10_mahalanobis)
print("Bottom 10 Mahalanobis distances:")
print(bottom_10_mahalanobis)


