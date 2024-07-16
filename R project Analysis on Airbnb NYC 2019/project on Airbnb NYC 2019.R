library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(corrplot)
#install.packages(c('plotly','reshape2','corrplot'))
# Read the CSV file into a data frame
data <- read.csv(file.choose())

# Create a copy of the data frame
df <- data

# Remove multiple columns and assign the result back to df
df <- df %>% select(-id, -name, -host_id, -host_name)
# Print the modified data frame
print(df)

# Check for missing values in each column
missing_values <- sapply(df, function(x) sum(is.na(x)))

# Print the number of missing values in each column
print(missing_values)

# Data cleaning
# Remove outliers for price and minimum_nights column

# Calculate z-scores
df <- df %>%
  mutate(z_price = abs(scale(price)),
         z_min_nights = abs(scale(minimum_nights)))

# Remove rows with z-scores greater than 3
df_clean <- df %>%
  filter(z_price < 3, price > 3, z_min_nights < 3)

# Convert numeric variables into categorical variables

# Minimum nights group
df_clean <- df_clean %>%
  mutate(minimum_nights_group = case_when(
    minimum_nights == 1 ~ "one night",
    minimum_nights == 2 ~ "two nights",
    minimum_nights == 3 ~ "three nights",
    minimum_nights == 4 ~ "four nights",
    minimum_nights > 4 ~ "five nights or more",
    TRUE ~ "Others"
  ))

# Calculated host listings count group
df_clean <- df_clean %>%
  mutate(calculated_host_listings_count_group = case_when(
    calculated_host_listings_count == 1 ~ "one listing",
    calculated_host_listings_count == 2 ~ "two listings",
    calculated_host_listings_count > 2 ~ "more than two listings",
    TRUE ~ "Others"
  ))

# Remove unused columns
df_clean <- df_clean %>%
  select(-z_price, -z_min_nights, -minimum_nights, -last_review, -neighbourhood,
         -calculated_host_listings_count, -reviews_per_month)

# Print the final cleaned data frame
print(df_clean)

# Ensure all columns are numeric before calculating the correlation matrix
numeric_columns <- df_clean %>%
  select_if(is.numeric)

# Compute correlations, excluding latitude and longitude
df_cor <- numeric_columns %>%
  select(-latitude, -longitude) %>%
  cor()

# Generate a mask for the upper triangle
mask <- upper.tri(df_cor)

# Plot heatmap using corrplot
corrplot(df_cor, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7)

# Generate a density map using plotly
lat <- mean(df_clean$latitude, na.rm = TRUE)
lon <- mean(df_clean$longitude, na.rm = TRUE)

fig <- plot_ly(
  data = df_clean, 
  lat = ~latitude, 
  lon = ~longitude, 
  z = ~price, 
  type = "densitymapbox",
  radius = 2
) %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lat = lat, lon = lon),
      zoom = 10
    )
  )

fig

# Group by neighbourhood_group and calculate summary statistics
summary_stats <- df_clean %>%
  group_by(neighbourhood_group) %>%
  summarise(
    price_count = n(),
    price_mean = mean(price, na.rm = TRUE),
    price_median = median(price, na.rm = TRUE),
    number_of_reviews_mean = mean(number_of_reviews, na.rm = TRUE),
    number_of_reviews_median = median(number_of_reviews, na.rm = TRUE),
    availability_365_mean = mean(availability_365, na.rm = TRUE),
    availability_365_median = median(availability_365, na.rm = TRUE)
  )

print(summary_stats)

# Boxplot of neighbourhood group and price
ggplot(df_clean, aes(x = neighbourhood_group, y = price, fill = room_type)) +
  geom_boxplot() +
  theme(legend.position = "top") +
  labs(title = "Boxplot of Price by Neighbourhood Group and Room Type")

# Boxplot of neighbourhood group and availability_365
ggplot(df_clean, aes(x = neighbourhood_group, y = availability_365, fill = room_type)) +
  geom_boxplot() +
  theme(legend.position = "top") +
  labs(title = "Boxplot of Availability by Neighbourhood Group and Room Type")

# Boxplot of minimum nights group and availability_365
ggplot(df_clean, aes(x = minimum_nights_group, y = availability_365)) +
  geom_boxplot() +
  labs(title = "Boxplot of Availability by Minimum Nights Group")

# Boxplot of minimum nights group and price
ggplot(df_clean, aes(x = minimum_nights_group, y = price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Price by Minimum Nights Group")

# Pairplot (scatterplot matrix) excluding latitude and longitude
numeric_df_clean <- df_clean %>% select_if(is.numeric) %>% select(-latitude, -longitude)
pairs(numeric_df_clean)

# Final model data frame
df_model <- df_clean %>%
  select(-latitude, -longitude)

# Print the final model data frame
print(df_model)

