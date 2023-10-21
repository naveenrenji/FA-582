# Load the required libraries
library(readr)
library(dplyr)
library(reshape2)
library(tidyr)

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

# Selecting rows for the top 100 tickers
fundamentals_2013_subset <- filter(fundamentals_2013, `Ticker Symbol` %in% top_100_tickers)

# selected columns
selected_columns <- c("Ticker Symbol", "After Tax ROE", "Cash Ratio", "Current Ratio", "Operating Margin", "Pre-Tax Margin", "Pre-Tax ROE", "Profit Margin", "Quick Ratio", "Total Assets", "Total Liabilities", "Earnings Per Share")
fundamentals_2013_subset_selected <- dplyr::select(fundamentals_2013_subset, all_of(selected_columns))

# Function for Lp-norm
lpnorm <- function(x, y, p) {
  return (sum(abs(x - y)^p)^(1 / p))
}

# Function for weighted Minkowski distance
weights <- c(0.0941, 0.0941, 0.0941, 0.0824, 0.0706, 0.0706, 0.1059, 0.1059, 0.0941, 0.0588) #calculation explained in the report
weighted_minkowski <- function(x, y, p) {
  return (sum(weights * abs(x - y)^p)^(1 / p))
}

# Calculate distances and sort and return as data frame
calc_and_sort_distances <- function(df, dist_func, p = NULL) {
  dist_list <- list()
  for (i in 1:(nrow(df) - 1)) {
    for (j in (i + 1):nrow(df)) {
      ticker1 <- as.character(df[i, 'Ticker Symbol'])
      ticker2 <- as.character(df[j, 'Ticker Symbol'])
      pair <- paste(ticker1, ticker2, sep = "-")
      if (is.null(p)) {
        dist <- dist_func(df[i, -1], df[j, -1])
      } else {
        dist <- dist_func(df[i, -1], df[j, -1], p)
      }
      dist_list[[pair]] <- dist
    }
  }
  sorted_list <- sort(unlist(dist_list))
  sorted_df <- data.frame(Ticker_Pair = names(sorted_list), Distance = sorted_list)
  return (sorted_df)
}

# Lp-norm calculations
for (p in c(1, 2, 3, 10)) {
  sorted_distances_df <- calc_and_sort_distances(fundamentals_2013_subset_selected, lpnorm, p)
  print(paste("Top 10 and Bottom 10 for Lp-norm with p =", p))
  print("Top 10:")
  print(head(sorted_distances_df, 10))
  print("Bottom 10:")
  print(tail(sorted_distances_df, 10))
}

# Minkowski distance calculations
sorted_distances_minkowski_df <- calc_and_sort_distances(fundamentals_2013_subset_selected, weighted_minkowski, 2)
print("Top 10 and Bottom 10 for Weighted Minkowski Distance")
print("Top 10:")
print(head(sorted_distances_minkowski_df, 10))
print("Bottom 10:")
print(tail(sorted_distances_minkowski_df, 10))



