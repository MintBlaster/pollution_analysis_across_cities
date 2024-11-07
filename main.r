library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(tseries)
library(ggcorrplot)

data <- read_csv("city_hour.csv")

data$Datetime <- ymd_hms(data$Datetime)

cleaned_data <- na.omit(data)

ggplot(cleaned_data, aes(x = City, y = PM2.5)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of PM2.5 Levels Across Cities", x = "City", y = "PM2.5 Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

major_cities <- c("Delhi", "Mumbai", "Kolkata", "Chennai", "Bengaluru")
city_data <- cleaned_data %>% filter(City %in% major_cities)

ggplot(city_data, aes(x = Datetime, y = `PM2.5`, color = City)) +
  geom_line() +
  labs(title = "PM2.5 Levels Over Time in Major Cities", x = "Date", y = "PM2.5 Level") +
  facet_wrap(~ City, ncol = 1)

cleaned_data$Season <- case_when(
  month(cleaned_data$Datetime) %in% c(12, 1, 2) ~ "Winter",
  month(cleaned_data$Datetime) %in% c(3, 4, 5) ~ "Summer",
  month(cleaned_data$Datetime) %in% c(6, 7, 8, 9) ~ "Monsoon",
  TRUE ~ "Autumn"
)

ggplot(cleaned_data, aes(x = Season, y = `PM2.5`, fill = City)) +
  geom_boxplot() +
  labs(title = "Seasonal Trends of PM2.5 Across Cities", x = "Season", y = "PM2.5 Level")

pollutants_data <- cleaned_data %>% select(`PM2.5`, `PM10`, NO2, SO2, O3, CO)

cor_matrix <- cor(pollutants_data, use = "complete.obs")
print(cor_matrix)

ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3)

cleaned_data$Hour <- hour(cleaned_data$Datetime)

hourly_avg <- cleaned_data %>% group_by(Hour, City) %>% summarize(avg_pm25 = mean(`PM2.5`, na.rm = TRUE))

ggplot(hourly_avg, aes(x = Hour, y = avg_pm25, color = City)) +
  geom_line() +
  labs(title = "Average PM2.5 Levels by Hour of the Day Across Cities", x = "Hour of Day", y = "Average PM2.5")

cleaned_data$Year <- year(cleaned_data$Datetime)

annual_avg <- cleaned_data %>% group_by(Year, City) %>% summarize(avg_pm25 = mean(`PM2.5`, na.rm = TRUE))

ggplot(annual_avg, aes(x = Year, y = avg_pm25, color = City)) +
  geom_line() +
  labs(title = "Annual Average PM2.5 Levels in Major Cities", x = "Year", y = "Average PM2.5")

top_cities <- cleaned_data %>%
  group_by(City) %>%
  summarize(avg_pm25 = mean(`PM2.5`, na.rm = TRUE)) %>%
  arrange(desc(avg_pm25)) %>%
  head(5)
print("Top 5 cities with highest average PM2.5 levels:")
print(top_cities)

seasonal_summary <- cleaned_data %>%
  group_by(Season, City) %>%
  summarize(avg_pm25 = mean(`PM2.5`, na.rm = TRUE))
print("Seasonal PM2.5 levels summary:")
print(seasonal_summary)

peak_hours <- hourly_avg %>%
  group_by(City) %>%
  filter(avg_pm25 == max(avg_pm25)) %>%
  select(City, Hour, avg_pm25)
print("Peak PM2.5 levels by hour for each city:")
print(peak_hours)
