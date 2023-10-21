# Load the required libraries
library(readr)
library(dplyr)
library(reshape2)

# Load the data from the CSV files
fundamentals_df <- read_csv("fundamentals.csv")
securities_df <- read_csv("securities.csv")

# Filter Data for Specific Year (2013)
fundamentals_2013 <- filter(fundamentals_df, substr(`Period Ending`, 1, 4) == "2013")
quantitative_cols <- c("After Tax ROE", "Cash Ratio", "Current Ratio", "Operating Margin", "Pre-Tax Margin",
                       "Pre-Tax ROE", "Profit Margin", "Quick Ratio", "Total Assets", "Total Liabilities", "Earnings Per Share")

# Select tickers with least missing or zero values in quantitative columns
fundamentals_2013 <- fundamentals_2013 %>% 
  rowwise() %>% 
  mutate(missing_or_zero = sum(across(all_of(quantitative_cols), ~is.na(.) | . == 0), na.rm = TRUE)) %>% 
  ungroup()

top_100_tickers <- fundamentals_2013 %>% 
  arrange(missing_or_zero) %>% 
  head(100) %>% 
  pull('Ticker Symbol')

fundamentals_2013_subset <- filter(fundamentals_2013, `Ticker Symbol` %in% top_100_tickers)
securities_subset <- filter(securities_df, `Ticker symbol` %in% top_100_tickers)

selected_columns <- c("Ticker Symbol", "After Tax ROE", "Cash Ratio", "Current Ratio", "Operating Margin", "Pre-Tax Margin", "Pre-Tax ROE", "Profit Margin", "Quick Ratio", "Total Assets", "Total Liabilities", "Earnings Per Share")
fundamentals_2013_subset_selected <- dplyr::select(fundamentals_2013_subset, all_of(selected_columns))

# Merge the two data frames
merged_df <- merge(fundamentals_2013_subset_selected, securities_subset, by.x = "Ticker Symbol", by.y = "Ticker symbol")

df_numeric <- merged_df[, c("After Tax ROE", "Cash Ratio", "Current Ratio", "Operating Margin", "Pre-Tax Margin",
                            "Pre-Tax ROE", "Profit Margin", "Quick Ratio", "Total Assets", "Total Liabilities", "Earnings Per Share")]
df_categorical <- merged_df[, c("Security", "SEC filings", "GICS Sector","GICS Sub Industry", "Address of Headquarters")]

lambda <- 0.8

#  categorical similarity (Overlap)
calc_cat_sim <- function(x, y) {
  return(ifelse(all(x == y), 1, 0))
}

# numerical similarity (Euclidean distance)
calc_num_sim <- function(x, y) {
  return(sum((x - y)^2))
}

num_tickers <- nrow(merged_df)
overall_sim_matrix <- matrix(0, nrow=num_tickers, ncol=num_tickers)
overall_sim_df <- data.frame(Var1 = character(), Var2 = character(), value = numeric())

ticker_symbols <- merged_df$`Ticker Symbol`

# overall similarity
for (i in 1:(num_tickers - 1)) {
  for (j in (i + 1):num_tickers) {
    if (i == j) {
      overall_sim_matrix[i, j] <- 1 #similarity score of 1 when tickers are  same
    } else {
      num_sim <- calc_num_sim(df_numeric[i, ], df_numeric[j, ])
      cat_sim <- calc_cat_sim(df_categorical[i, ], df_categorical[j, ])
      overall_sim <- lambda * num_sim + (1 - lambda) * cat_sim
      overall_sim_df <- rbind(overall_sim_df, data.frame(Var1 = ticker_symbols[i], Var2 = ticker_symbols[j], value = overall_sim))
    }
  }
}

ticker_symbols <- merged_df$`Ticker Symbol`
rownames(overall_sim_matrix) <- ticker_symbols
colnames(overall_sim_matrix) <- ticker_symbols

# Rank the similarities
overall_sim_df$rank <- rank(-overall_sim_df$value)
top_10_sim <- overall_sim_df[order(-overall_sim_df$value), ][1:10,]
bottom_10_sim <- overall_sim_df[order(overall_sim_df$value), ][1:10,]
similarity_results <- (list(top_10 = top_10_sim, bottom_10 = bottom_10_sim))

print("Top 10 similarities:")
print(similarity_results$top_10)

print("Bottom 10 similarities:")
print(similarity_results$bottom_10)
