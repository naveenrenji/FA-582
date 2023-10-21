library(dplyr)
library(ggplot2)

# Load and combine data
nyt1 <- read.csv("nyt1.csv")
nyt1$Day <- "Day1"

nyt2 <- read.csv("nyt2.csv")
nyt2$Day <- "Day2"

nyt3 <- read.csv("nyt3.csv")
nyt3$Day <- "Day3"

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

# Plotting
p1 <- ggplot(nyt_summary_imp, aes(x = age_group, y = mean_impressions, color = Day, group = Day)) +
  geom_line() +
  labs(title = "Variation in Average Impressions by Age Group and Day", x = "Age Group", y = "Average Impressions")

p2 <- ggplot(nyt_summary_CTR, aes(x = age_group, y = mean_CTR, color = Day, group = Day)) +
  geom_line() +
  labs(title = "Variation in Average Click-Through Rate (CTR) by Age Group and Day", x = "Age Group", y = "Average CTR")

p3 <- ggplot(nyt_data, aes(x = age_group, y = CTR, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Distribution of CTR Across Age Groups", x = "Age Group", y = "CTR")

p4 <- ggplot(nyt_data, aes(x = CTR, colour = age_group)) +
  geom_density() +
  labs(title = "Density Plot of CTR Across Age Groups", x = "CTR", y = "Density")

# Display plots
p1
p2
p3
p4
