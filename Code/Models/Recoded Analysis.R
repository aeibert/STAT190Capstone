#Libraries
#---- 
rm(list= ls())
library(car)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(randomForest)
library(pROC)
library(forecast)
library(tsibble)
library(fable)
library(feasts)
library(RColorBrewer)
library(dplyr)
library(scales)

setwd(#Place path to working directory here)
all <- read.csv(#Place name of data file here)
updated_data <- read.csv(#Place updated date file name here)

#----
#Data
#----
all <- all %>%
  mutate(
    served_date = ymd(served_date),
    dob = ymd(dob)
  )

visits <- all %>%
  mutate(
    served_date = ymd(served_date),
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month")
  ) %>%
  group_by(round_month) %>%
  filter(served_year != 2018 & !(served_year == 2024))

all_updated$servedDate <- as.Date(all_updated$servedDate)

visits_updated <- all_updated %>%
  mutate(
    updated_served_year = year(servedDate),
    updated_served_month = month(servedDate),
    updated_served_day_of_month = mday(servedDate),
    updated_round_month = round_date(servedDate, "month")
  ) %>%
  group_by(updated_round_month) %>%
  filter(updated_served_year != 2018) 

#----
#Pantry Visits Over Time
#----
# Create a chart showing the number of first time visits over time
ggplot(visits, aes(x = round_month)) +
  geom_bar() +
  labs(title = "Number of Pantry Visits (Monthly, Excluding 2018)",
       x = "Month", y = "Number of Visits") + theme_minimal()

# Create a chart showing the number of first time visits over time with updated data
ggplot(visits_updated, aes(x = updated_round_month)) +
  geom_bar() +
  labs(title = "Number of Pantry Visits (Monthly, Excluding 2018)",
       x = "Month", y = "Number of Visits") + theme_minimal()
#----
#Agregating Data for Predictions
#----
# Aggregate the data to get the number of visitors per month
visits_monthly <- visits %>%
  group_by(round_month) %>%
  summarise(
    new_visits = n()  # Number of visits per month
  )

visits_monthly <- visits_monthly %>%
  mutate(
    month = month(round_month),
    year = year(round_month),
    quarter = quarter(round_month),
    day_of_week = weekdays(round_month)
  )

# Fit a linear regression model
lm_model <- lm(new_visits ~ month + year + quarter + day_of_week, data = visits_monthly)

# Predict the number of visitors for future months (create a new dataset with future months)
future_data <- data.frame(
  month = c(1, 2, 3), 
  year = c(2025, 2025, 2025),
  quarter = c(1, 1, 1),
  day_of_week = c('Monday', 'Tuesday', 'Wednesday')
)

# Predict visits for the future months
predictions <- predict(lm_model, newdata = future_data)

# Model performance metrics
predicted_values <- predict(lm_model)
actual_values <- visits_monthly$new_visits

# Calculate MSE
MSE <- mean((predicted_values - actual_values)^2)

# Check R-squared value
summary(lm_model)$r.squared

# Predict the number of visitors for each month in 2024
future_data_2024 <- data.frame(
  month = rep(1:12, each = 1),  
  year = rep(2024, 12), 
  quarter = rep(1:4, length.out = 12),
  day_of_week = weekdays(as.Date(paste(2024, 1:12, 1, sep = "-")))  
)

# Predict visits for the months of 2024
predictions_2024 <- predict(lm_model, newdata = future_data_2024)

predicted_2024_df <- data.frame(
  round_month = as.Date(paste(2024, future_data_2024$month, 1, sep = "-")),
  predicted_visits = predictions_2024,
  year = 2024,
  month = future_data_2024$month
)

#----
#Predicting 2024
#----
predictions_2024 <- predict(lm_model, newdata = future_data_2024)

predicted_2024_df <- data.frame(
  round_month = as.Date(paste(2024, future_data_2024$month, 1, sep = "-")),
  predicted_visits = predictions_2024,
  year = 2024,
  month = future_data_2024$month
)

ggplot() +
  # Plot observed data
  geom_line(data = visits_monthly, aes(x = round_month, y = new_visits), color = 'blue') +
  # Plot predicted data for 2024
  geom_line(data = predicted_2024_df, aes(x = round_month, y = predicted_visits), color = 'red', linetype = "dashed") +
  labs(title = "Observed and Predicted Visitors", x = "Year", y = "Number of Visitors") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  # Format X-axis to show only the year
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate month labels for better spacing if needed
    legend.title = element_blank()
  )

# Combine observed data and predicted data
observed_2024 <- visits_monthly %>%
  filter(year == 2024)  # Filter only the data for 2024

#Zoomed in Predictions for 2024
ggplot() +
  geom_line(data = observed_2024, aes(x = round_month, y = new_visits), color = 'blue', size = 1) +
  geom_line(data = predicted_2024_df, aes(x = round_month, y = predicted_visits), color = 'red', linetype = "dashed", size = 1) +
  labs(title = "Predicted Visitors for 2024", x = "Month", y = "Number of Visitors") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +  # Format X-axis to show month names
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate month labels for better spacing
    legend.title = element_blank()
  )

#----
#Sarima Forecast
#----
# Aggregate the data to get the number of new visitors per month
visits_monthly <- visits %>%
  group_by(round_month) %>%
  summarise(
    new_visits = n()  # Number of new visits (first-time visits) per month
  )

# Convert round_month to Date class and extract relevant time variables
visits_monthly <- visits_monthly %>%
  mutate(
    round_month = as.Date(round_month),
    month = month(round_month),
    year = year(round_month),
    quarter = quarter(round_month),
    day_of_week = weekdays(round_month)
  )

# Create time series object for SARIMA model
ts_data <- ts(visits_monthly$new_visits, frequency = 12, start = c(min(visits_monthly$year), min(visits_monthly$month)))

# Fit SARIMA model (Seasonal ARIMA)
sarima_model <- auto.arima(ts_data, seasonal = TRUE)

# Forecast new visits for 2024 using the SARIMA model
sarima_forecast <- forecast(sarima_model, h = 12)
#----
#Creating Future Data
#----
future_data_2024 <- data.frame(
  month = 1:12,
  year = 2024,
  quarter = rep(1:4, length.out = 12),
  day_of_week = weekdays(as.Date(paste(2024, 1:12, 1, sep = "-"))),
  round_month = as.Date(paste(2024, 1:12, 1, sep = "-"))
)

# Generate predicted values using the linear regression model
lm_model <- lm(new_visits ~ month + year + quarter + day_of_week, data = visits_monthly)
predictions_lm <- predict(lm_model, newdata = future_data_2024)

# Create data frame for predictions (SARIMA vs LM)
predicted_2024_df <- data.frame(
  round_month = as.Date(paste(2024, future_data_2024$month, 1, sep = "-")),
  predicted_visits_sarima = sarima_forecast$mean,
  predicted_visits_lm = predictions_lm
)

sarima_forecast$mean
#----
#Comparing Models
#----

#Create a graph with the new observed 2024 data along side the base prediction model,
# and Sarima seasonality model

#Aggregate Observed Visits

# Count number of new households (first visit) per month in 2024
observed_2024 <- visits_updated %>%
  filter(updated_served_year == 2024) %>%
  group_by(updated_round_month) %>%
  summarise(visits = n_distinct(houseHoldIdAfn)) %>%
  rename(round_month = updated_round_month) %>%
  mutate(type = "Observed")

#Prep prediction datasets with similar structures

# Base prediction (no seasonality)
base_model <- predicted_2024_df %>%
  select(round_month, predicted_visits_lm) %>%
  rename(visits = predicted_visits_lm) %>%
  mutate(type = "Base Prediction (No Seasonality)")

# SARIMA model (renamed to future_data_2024)
sarima_model <- predicted_2024_df %>%
  select(round_month, predicted_visits_sarima) %>%
  rename(visits = predicted_visits_sarima) %>%
  mutate(type = "SARIMA Model")

#Combine all models into singular data frame
plot_data <- bind_rows(observed_2024, base_model, sarima_model)

#Plotting the data
cb_palette <- c(
  "Observed" = "#0072B2",
  "Base Prediction (No Seasonality)" = "#D55E00",
  "SARIMA Model" = "#E69F00")

line_types <- c(
  "Observed" = "solid",
  "Base Prediction (No Seasonality)" = "dashed",
  "SARIMA Model" = "twodash"
)

ggplot(plot_data, aes(x = round_month, y = visits, color = type, linetype = type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = cb_palette) +
  scale_linetype_manual(values = line_types) +
  scale_x_date(
    date_labels = "%b",
    breaks = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "1 month")
  ) +
  labs(
    title = "Observed and Predicted Visitors in 2024",
    x = "Month",
    y = "Number of New Visitors",
    color = "Legend",
    linetype = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
#----
# Plot of Visitors per month for different years
#----
ggplot(data = visits_monthly, aes(x = month, y = new_visits, color = factor(year), group = year)) +
  geom_line() +  # Plotting the line graph for each year
  labs(title = "Monthly Number of New Visitors", x = "Month", y = "Number of New Visitors") +
  scale_x_continuous(breaks = 1:12, labels = month.name) +  # Show month names on X-axis
  scale_color_brewer(palette = "Set2") +  # Use a colorblind-friendly palette
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate month labels for better spacing
    legend.title = element_blank(),  # Remove legend title
    legend.position = "top"
  )
#----
#Accuracy Testing for Models
#----

train <- visits_monthly %>% filter(year < 2023)
test <- visits_monthly %>% filter(year == 2023)

# Fit SARIMA on training set
ts_train <- ts(train$new_visits, frequency = 12, start = c(min(train$year), min(train$month)))
sarima_model_train <- auto.arima(ts_train, seasonal = TRUE)

# Forecast for 12 months (2023)
sarima_forecast_test <- forecast(sarima_model_train, h = 12)

# Compare predictions to actuals
predicted_test <- data.frame(
  round_month = test$round_month,
  predicted = sarima_forecast_test$mean,
  actual = test$new_visits
)

# Plot predictions vs actuals
ggplot(predicted_test, aes(x = round_month)) +
  geom_line(aes(y = actual), color = "blue") +
  geom_line(aes(y = predicted), color = "red", linetype = "dashed") +
  labs(title = "SARIMA Prediction vs Actual (2023)", y = "Visits", x = "Month") +
  theme_minimal()

# Root Mean Square Error (RMSE) and Mean Absolute Error (MAE)
rmse <- sqrt(mean((predicted_test$predicted - predicted_test$actual)^2))
mae <- mean(abs(predicted_test$predicted - predicted_test$actual))

rmse
mae


#----





