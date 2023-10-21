#Naveen Renji CWID - 20016323

#Problem 2 - 

#For plotting, it takes some time atleast on my laptop,
#please give it some time, and incase the error 'server error - invalid plot index' comes, 
#its beacuse r studio took too long to plot, but it will work if you just wait and try

# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Load data and add a Day column
nyt1 <- read.csv("nyt1.csv")
nyt1$Day <- "Day1"

nyt2 <- read.csv("nyt2.csv")
nyt2$Day <- "Day2"

nyt3 <- read.csv("nyt3.csv")
nyt3$Day <- "Day3"

# Combine datasets
nyt_data <- rbind(nyt1, nyt2, nyt3)

# Data Cleaning
nyt_data <- nyt_data %>%
  filter(Impressions != 0) %>%
  mutate(
    Gender = as.factor(Gender),
    age_group = cut(Age, breaks = c(-Inf, 19, 29, 39, 49, 59, 69, Inf), labels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")),
    median_age = median(Age[Age >= 1 & Age <= 90], na.rm = TRUE),
    Age = ifelse(Age < 1 | Age > 90, median_age, Age),
    CTR = Clicks / Impressions,
    click_category = case_when(
      Impressions == 0 ~ "No Impressions",
      Clicks == 0 ~ "No Clicks",
      Clicks > 0 ~ "Clicked",
      TRUE ~ "Unknown"
    )
  )

# Data summary for CTR
nyt_summary_CTR <- nyt_data %>%
  group_by(Day, age_group) %>%
  summarise(mean_CTR = mean(CTR, na.rm = TRUE))

# Data summary for Impressions
nyt_summary_imp <- nyt_data %>%
  group_by(Day, age_group) %>%
  summarise(mean_impressions = mean(Impressions, na.rm = TRUE))

# creating categories based on CTR - doing a split by 5
nyt_data <- nyt_data %>%
  mutate(
    CTR_category = case_when(
      CTR == 0 ~ "No Clicks",
      CTR > 0 & CTR <= 0.2 ~ "Low",
      CTR > 0.2 & CTR <= 0.5 ~ "Moderate",
      CTR > 0.5 & CTR <= 0.75 ~ "High",
      CTR > 0.75 & CTR <= 1 ~ "VeryHigh",
      TRUE ~ "Unknown"
    )
  )

# Compare <20-year-old males vs. <20-year-old females for CTR
ggplot(subset(nyt_data, age_group == "<20"), aes(x = as.factor(Gender), y = CTR)) +
  geom_point(aes(color = CTR_category), alpha = 0.6, position = "jitter") +
  ggtitle("CTR for <20-year-old Males vs Females") +
  xlab("Gender (0=Female, 1=Male)") +
  ylab("CTR") +
  facet_wrap(~ Day)


# Compare Logged-in vs Not Logged-in for CTR
ggplot(nyt_data, aes(x = as.factor(Signed_In), y = CTR)) +
  geom_point(aes(color = CTR_category), alpha = 0.6, position = "jitter") +
  ggtitle("CTR for Logged-in vs Not Logged-in") +
  xlab("Logged-in Status") +
  ylab("CTR") +
  facet_wrap(~ Day)

# Calculate average CTR for each age group
nyt_data_avg <- nyt_data %>%
  group_by(age_group) %>%
  summarise(avg_CTR = mean(CTR, na.rm = TRUE))

# Visualize the average CTR for each age group
ggplot(nyt_data_avg, aes(x = age_group, y = avg_CTR)) +
  geom_col() +
  ggtitle("Average CTR by Age Group") +
  xlab("Age Group") +
  ylab("Average CTR")

# Calculate average CTR for each gender
nyt_data_gender_avg <- nyt_data %>%
  group_by(Gender) %>%
  summarise(avg_CTR = mean(CTR, na.rm = TRUE))

# Visualize the average CTR for each gender
ggplot(nyt_data_gender_avg, aes(x = as.factor(Gender), y = avg_CTR)) +
  geom_col() +
  ggtitle("Average CTR by Gender") +
  xlab("Gender (0=Female, 1=Male)") +
  ylab("Average CTR")

# Calculate average CTR for each day
nyt_data_day_avg <- nyt_data %>%
  group_by(Day) %>%
  summarise(avg_CTR = mean(CTR, na.rm = TRUE))

# Visualize the average CTR for each day
ggplot(nyt_data_day_avg, aes(x = Day, y = avg_CTR)) +
  geom_col() +
  ggtitle("Average CTR by Day") +
  xlab("Day") +
  ylab("Average CTR")


ggplot(nyt_data, aes(x = age_group, y = Impressions)) +
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("Average Impressions by Age Group Over Time") +
  xlab("Age Group") +
  ylab("Average Impressions") +
  facet_wrap(~ Day)

# Plotting
ggplot(nyt_summary_imp, aes(x = age_group, y = mean_impressions, color = Day, group = Day)) +
  geom_line() +
  labs(title = "Variation in Average Impressions by Age Group and Day", x = "Age Group", y = "Average Impressions")

ggplot(nyt_summary_CTR, aes(x = age_group, y = mean_CTR, color = Day, group = Day)) +
  geom_line() +
  labs(title = "Variation in Average Click-Through Rate (CTR) by Age Group and Day", x = "Age Group", y = "Average CTR")

ggplot(nyt_data, aes(x = age_group, y = CTR, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Distribution of CTR Across Age Groups", x = "Age Group", y = "CTR")

ggplot(nyt_data, aes(x = CTR, colour = age_group)) +
  geom_density() +
  labs(title = "Density Plot of CTR Across Age Groups", x = "CTR", y = "Density")



