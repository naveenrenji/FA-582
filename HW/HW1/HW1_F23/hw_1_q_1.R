# Load the package
library(openxlsx)
library(ggplot2)

# Read the xlsx files
manhattan <- read.xlsx("rollingsales_manhattan.xlsx", sheet = 1, startRow = 5)
bronx <- read.xlsx("rollingsales_bronx.xlsx", sheet = 1, startRow = 5)
brooklyn <- read.xlsx("rollingsales_brooklyn.xlsx", sheet = 1, startRow = 5)
queens <- read.xlsx("rollingsales_queens.xlsx", sheet = 1, startRow = 5)
statenisland <- read.xlsx("rollingsales_statenisland.xlsx", sheet = 1, startRow = 5)

# Load dplyr for data manipulation
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Add BOROUGH column to each data frame
manhattan <- mutate(manhattan, BOROUGH = 'Manhattan')
bronx <- mutate(bronx, BOROUGH = 'Bronx')
brooklyn <- mutate(brooklyn, BOROUGH = 'Brooklyn')
queens <- mutate(queens, BOROUGH = 'Queens')
statenisland <- mutate(statenisland, BOROUGH = 'Staten Island')

# Combine all data frames
data <- rbind(manhattan, bronx, brooklyn, queens, statenisland)

#head(data)
#tail(data)
#summary(data)

unique(data$BUILDING.CLASS.CATEGORY)
#str(data)

# Convert all column names to lowercase
colnames(data) <- tolower(colnames(data))

# Take a look at the first few rows to confirm that column names

data$sale.date <- convertToDateTime(as.numeric(data$sale.date))
#data$sale.date


# Check for missing (NULL or NA) values in the data frame
missing_vals <- sapply(data, function(x) sum(is.na(x)))
print(paste("Missing values in each column:"))
print(missing_vals)

# Check the number of rows with sale.price equal to 0
zero_sales <- sum(data$sale.price == 0)
print(paste("Number of rows with zero sale price:", zero_sales))

# Remove rows where sale.price is zero
data <- subset(data, sale.price != 0)

# Display the first few rows to confirm the removal
#head(data)

#Removing outliers

# Calculate the IQR for sale.price
Q1 <- quantile(data_clean$sale.price, 0.25)
Q3 <- quantile(data_clean$sale.price, 0.75)
IQR <- Q3 - Q1

# Define the upper and lower bounds for outliers
upper_bound <- Q3 + 1.5 * IQR
lower_bound <- Q1 - 1.5 * IQR

# Count the number of outliers
outliers_count <- nrow(subset(data_clean, sale.price < lower_bound | sale.price > upper_bound))

# Print the count of outliers
print(paste("Number of outliers: ", outliers_count))

# Remove outliers
data <- subset(data_clean, sale.price >= lower_bound & sale.price <= upper_bound)

# Display the first few rows to confirm the removal
#head(data)

# Calculate the median value for 'year.built' excluding zeros
median_year_built <- median(data$year.built[data$year.built != 0], na.rm = TRUE)

# Replace zero values in 'year.built' with the median value
data$year.built[data$year.built == 0] <- median_year_built


# Stripping leading and trailing whitespace in 'building.class.category' column before filtering
data$building.class.category <- trimws(data$building.class.category)

# Define the categories to consider
categories_to_consider <- c(
  '01  ONE FAMILY HOMES', '02  TWO FAMILY HOMES', '03  THREE FAMILY HOMES',
  '09  COOPS - WALKUP APARTMENTS', '10  COOPS - ELEVATOR APARTMENTS', 
  '12  CONDOS - WALKUP APARTMENTS', '13  CONDOS - ELEVATOR APARTMENTS', 
  '15  CONDOS - 2-10 UNIT RESIDENTIAL'
)

# Filter the data based on the categories
filtered_data <- subset(data, building.class.category %in% categories_to_consider)

# Show the dimensions of the filtered data
dim(filtered_data)
unique(filtered_data$building.class.category)

#Comparison of Sale Prices Across Boroughs
ggplot(filtered_data, aes(x=borough, y=sale.price)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Comparison of Sale Prices Across Boroughs",
       x = "Borough",
       y = "Sale Price")


# Sale Prices Across Different Building Categories
ggplot(filtered_data, aes(x=building.class.category, y=sale.price)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Sale Prices Across Different Building Categories") +
  xlab("Building Categories") +
  ylab("Sale Price")


#Building Categories Across Different Boroughs
ggplot(filtered_data, aes(x = building.class.category, fill = borough)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Building Categories Across Different Boroughs",
       x = "Building Class Category",
       y = "Count") +
  scale_fill_brewer(palette = "Set1")

#Sale Price Over Time Across Boroughs
ggplot(filtered_data, aes(x = sale.date, y = sale.price, color = borough)) +
  geom_line() +
  geom_smooth(method = 'loess') +  # Adding a smoothed line for trend
  labs(title = "Sale Price Over Time Across Boroughs",
       x = "Sale Date",
       y = "Sale Price") +
  scale_color_brewer(palette = "Set1")


