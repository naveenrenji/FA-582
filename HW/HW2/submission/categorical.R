library(dplyr)
library(readr)
library(tidyr)

# Read the data
securities_df <- read_csv("securities.csv")
categorical_cols <- c("GICS Sector", "GICS Sub Industry", "Address of Headquarters")

# Preprocess the data
securities_df <- securities_df %>%
  rowwise() %>%
  mutate(missing_or_zero = sum(across(all_of(categorical_cols), ~is.na(.) | . == 0), na.rm = TRUE)) %>%
  ungroup()

# Select top 100 tickers
top_100_tickers <- securities_df %>%
  arrange(missing_or_zero) %>%
  head(100) %>%
  pull('Ticker symbol')

securities_subset <- filter(securities_df, `Ticker symbol` %in% top_100_tickers)

# Convert all categorical columns to character type
securities_subset <- securities_subset %>%
  mutate(across(all_of(categorical_cols), as.character))

# overlap similarity
overlap_similarity <- function(x, y) {
  if (is.na(x) || is.na(y)) return(0)
  if (x == y) return(1)
  return(0)
}

# inverse frequency similarity
inverse_frequency_similarity <- function(x, y, p) {
  if (is.na(x) || is.na(y)) return(0)
  if (x == y) return(1 / (p^2))
  return(0)
}

#Goodall similarity
goodall_similarity <- function(x, y, p) {
  if (is.na(x) || is.na(y)) return(0)
  if (x == y) return(1 - (p^2))
  return(0)
}

# Calculate pk(x) for each categorical column
pk_values <- lapply(categorical_cols, function(col) {
  table(securities_subset[[col]]) / nrow(securities_subset)
})

# empty matrix to store similarity scores
n <- nrow(securities_subset)
tickers <- as.character(securities_subset$`Ticker symbol`)

overlap_matrix <- matrix(0, n, n, dimnames=list(tickers, tickers))
inverse_frequency_matrix <- matrix(0, n, n, dimnames=list(tickers, tickers))
goodall_matrix <- matrix(0, n, n, dimnames=list(tickers, tickers))

# Calculate similarity for each pair of tickers
for (i in 1:n) {
  for (j in 1:n) {
    if (i == j) next # Skip diagonal values
    for (col in 1:length(categorical_cols)) {
      xi <- securities_subset[i, categorical_cols[col]]
      yi <- securities_subset[j, categorical_cols[col]]
      p <- pk_values[[col]][as.character(xi)]
      
      overlap_matrix[i, j] <- overlap_matrix[i, j] + overlap_similarity(xi, yi)
      inverse_frequency_matrix[i, j] <- inverse_frequency_matrix[i, j] + inverse_frequency_similarity(xi, yi, p)
      goodall_matrix[i, j] <- goodall_matrix[i, j] + goodall_similarity(xi, yi, p)
    }
  }
}

# Convert matrices to data frames for easier sorting and viewing
overlap_df <- as.data.frame(as.table(overlap_matrix))
inverse_frequency_df <- as.data.frame(as.table(inverse_frequency_matrix))
goodall_df <- as.data.frame(as.table(goodall_matrix))

# Print top 10 and bottom 10 similarities, excluding diagonal and duplicate pairs
print("Top 10 Overlap Similarities")
print(head(overlap_df %>% filter(as.character(Var1) != as.character(Var2) & as.character(Var1) < as.character(Var2)) %>% arrange(desc(Freq)), 10))

print("Bottom 10 Overlap Similarities")
print(head(overlap_df %>% filter(as.character(Var1) != as.character(Var2) & as.character(Var1) < as.character(Var2)) %>% arrange(Freq), 10))

print("Top 10 Inverse Frequency Similarities")
print(head(inverse_frequency_df %>% filter(as.character(Var1) != as.character(Var2) & as.character(Var1) < as.character(Var2)) %>% arrange(desc(Freq)), 10))

print("Bottom 10 Inverse Frequency Similarities")
print(head(inverse_frequency_df %>% filter(as.character(Var1) != as.character(Var2) & as.character(Var1) < as.character(Var2)) %>% arrange(Freq), 10))

print("Top 10 Goodall Similarities")
print(head(goodall_df %>% filter(as.character(Var1) != as.character(Var2) & as.character(Var1) < as.character(Var2)) %>% arrange(desc(Freq)), 10))

print("Bottom 10 Goodall Similarities")
print(head(goodall_df %>% filter(as.character(Var1) != as.character(Var2) & as.character(Var1) < as.character(Var2)) %>% arrange(Freq), 10))
