# Load required libraries
library(httr)
library(rvest)
library(tidyverse)

# Define the URL
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# Fetch the webpage using httr
response <- GET(url)

# Parse the webpage content
webpage <- read_html(content(response, "text"))

# Extract the table (Note: This code is for educational purposes only)
sp500_table <- html_nodes(webpage, "table")[[1]] %>% html_table()

# Print a preview of the table
print(head(sp500_table))

# Perform Exploratory Data Analysis (EDA)
# Summary statistics
summary(sp500_table)

str(sp500_table)
#formatting Date
sp500_table$`Date added` <- as.Date(sp500_table$`Date added`, format="%Y-%m-%d")
str(sp500_table)
#formatting founded
# Extract the first set of four digits as the year
sp500_table$Founded_cleaned <- stringr::str_extract(sp500_table$Founded, "\\d{4}")

# Convert the cleaned "Founded" column to numeric
sp500_table$Founded_numeric <- as.numeric(sp500_table$Founded_cleaned)

#Distribution of Companies by Founding Year
ggplot(sp500_table, aes(x=Founded_numeric)) +
  geom_histogram(binwidth=10, aes(fill=..count..)) +
  ggtitle("Distribution of Companies by Founding Year") +
  xlab("Founded Year") +
  ylab("Number of Companies")

# Create a histogram of the 'Date added' column, ignoring NA values
ggplot(data = sp500_table, aes(x = `Date added`)) +
  geom_histogram(binwidth = 365.25, fill = 'blue', alpha = 0.7) +
  scale_x_date(breaks = "10 years", date_labels = "%Y") +
  labs(title = "Companies Added to S&P 500 Over the Years",
       x = "Year",
       y = "Number of Companies Added")


# checking companies adding when sp was founded in 1957
selected_companies <- sp500_table[sp500_table$`Date added` == "1957-03-04", ]
# Print the companies
print(selected_companies$Security)


#Years Taken to Get Added to S&P 500 by Year Founded

sp500_table$Year_added <- as.integer(format(as.Date(sp500_table$`Date added`), "%Y"))
sp500_table$Years_to_add <- sp500_table$Year_added - sp500_table$Founded_numeric


library(ggplot2)
ggplot(sp500_table, aes(x=Years_to_add)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7) +
  ggtitle("Time to Get Added to S&P 500") +
  xlab("Years from Founded to Added") +
  ylab("Number of Companies")


#Number of Companies by GICS Sector
ggplot(sp500_table, aes(x=`GICS Sector`)) +
  geom_bar(aes(fill=`GICS Sector`), position='dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Number of Companies by GICS Sector") +
  xlab("GICS Sector") +
  ylab("Number of Companies")

#Top 10 Most Popular Sub-Industries
# Count the occurrences of each sub-industry
sub_industry_counts <- table(sp500_table$`GICS Sub-Industry`)

# Sort in descending order and take the top 5 sub-industries
top_10_sub_industries <- names(sort(sub_industry_counts, decreasing=TRUE)[1:10])

# Filter the data frame to only include these top 10 sub-industries
filtered_sp500_table <- sp500_table[sp500_table$`GICS Sub-Industry` %in% top_10_sub_industries, ]

# Plot using ggplot2
ggplot(filtered_sp500_table, aes(x=`GICS Sub-Industry`)) +
  geom_bar(aes(fill=`GICS Sub-Industry`), position='dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 Most Popular Sub-Industries") +
  xlab("GICS Sub-Industry") +
  ylab("Number of Companies")

#Top 10 Company Headquarters Locations
# Count the occurrences of each headquarters location
hq_counts <- table(sp500_table$`Headquarters Location`)

# Sort in descending order and take the top 10 headquarters locations
top_10_hq <- names(sort(hq_counts, decreasing=TRUE)[1:10])

# Filter the data frame to only include these top 10 headquarters locations
filtered_sp500_table_hq <- sp500_table[sp500_table$`Headquarters Location` %in% top_10_hq, ]

# Plot using ggplot2
ggplot(filtered_sp500_table_hq, aes(x=`Headquarters Location`)) +
  geom_bar(aes(fill=`Headquarters Location`), position='dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Top 10 Company Headquarters Locations") +
  xlab("Headquarters Location") +
  ylab("Number of Companies")


#Summary Statistics for the entire data 
summary(sp500_table)




