# Load the required libraries
library(readr)
library(dplyr)

# Load the data from the CSV files
fundamentals_df <- read_csv("fundamentals.csv")
securities_df <- read_csv("securities.csv")
print("Structure of fundamentals_df:")
str(fundamentals_df)

# Filter Data for Specific Year (2013)
fundamentals_2013 <- filter(fundamentals_df, substr(`Period Ending`, 1, 4) == "2013")
quantitative_cols <- c("After Tax ROE", "Cash Ratio", "Current Ratio", "Operating Margin", "Pre-Tax Margin",
                       "Pre-Tax ROE", "Profit Margin", "Quick Ratio", "Total Assets", "Total Liabilities", "Earnings Per Share")

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

bucketize <- function(column, n_buckets) {
  breaks <- quantile(column, probs = seq(0, 1, length.out = n_buckets + 1))
  cut(column, breaks = breaks, labels = FALSE, include.lowest = TRUE)
}

match_score <- function(row1, row2) {
  sum(row1 == row2)
}

# Bucketize each column, 3 buckets
bucketized_data <- as.data.frame(lapply(fundamentals_2013_subset_selected[-1], bucketize, n_buckets=3))

# match-based similarity matrix
n <- nrow(bucketized_data)
match_similarity_matrix <- matrix(0, n, n)
rownames(match_similarity_matrix) <- fundamentals_2013_subset_selected$`Ticker Symbol`
colnames(match_similarity_matrix) <- fundamentals_2013_subset_selected$`Ticker Symbol`

for (i in 1:(n - 1)) {
  for (j in (i + 1):n) {
    score <- match_score(bucketized_data[i,], bucketized_data[j,])
    match_similarity_matrix[i, j] <- score
    match_similarity_matrix[j, i] <- score
  }
}

match_similarity_df <- as.data.frame(as.table(match_similarity_matrix))

# Filter out the diagonal and duplicate entries
match_similarity_df <- match_similarity_df %>% filter(Var1 != Var2)

# Sort by similarity score (Freq) in descending order for top 10 pairs
sorted_df_top <- match_similarity_df %>% arrange(desc(Freq))
print("Top 10 most similar pairs based on match score:")
print(head(sorted_df_top, 10))

# Sort by similarity score (Freq) in ascending order for bottom 10 pairs
sorted_df_bottom <- match_similarity_df %>% arrange(Freq)
print("Bottom 10 least similar pairs based on match score:")
print(head(sorted_df_bottom, 10))
