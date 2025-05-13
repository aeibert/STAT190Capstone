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
#Notes on the data
#----
#afn is household ID
#served data -> date come to pantry (year-month-day)
#fed_poverty_level -> what percent of the federal poverty level the individual is
#IMM is first initial of their last name
#dob -> date of birth
#family type gives profile on family

dim(all)
head(all)
#----
#Data Cleaning
#----

#little bit of cleaning dates
all <- all %>%
  mutate(
    served_date = ymd(served_date),
    dob = ymd(dob)
  )
#57 failed to parse - will have to deal with these on a case by case basis

updated_data$servedDate <- as.Date(updated_data$servedDate)

updated_data <- updated_data %>%
  mutate(
    servedDate = ymd(servedDate),
    dob = ymd(dob)
  )

#----
#Create a visit level data set and 
# Visualization of First Time Visits
#----

visit <- all %>%
  group_by(afn) %>%
  filter(served_date == min(served_date)) %>% # Keep only the earliest served_date per afn
  summarise(
    served_date = first(served_date),
    n_household = n(),
    zip = first(zip),
    annual_income = first(annual_income),
    fed_poverty_level = first(fed_poverty_level),
    gender = first(gender),
    race = first(race),
    ethnicity = first(ethnicity),
    education = first(education),
    family_type = first(family_type),
    location = first(location),
    snap_household = first(snap_household)
  ) %>%
  mutate(
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month")
  ) %>%
  filter(served_year != 2018 & !(served_year == 2024)) 
# Exclude February 2024 due to observations only going to 6th of the month

#Remove 2018 observations due to any visits recorded being marked new as this
# is when the data started being collected

visit_updated <- updated_data %>%
  group_by(houseHoldIdAfn) %>%
  filter(servedDate == min(servedDate)) %>% # Keep only the earliest served_date per afn
  summarise(
    servedDate = first(servedDate),
    householdMembers = n(),
    annualIncome = first(annualIncome),
    fedPovertyLevel = first(fedPovertyLevel),
    gender = first(gender),
    race = first(race),
    education = first(education),
    category = first(category),
    location = first(location),
    foodstamps = first(foodstamps)
  ) %>%
  mutate(
    updated_served_year = year(servedDate),
    updated_served_month = month(servedDate),
    updated_served_day_of_month = mday(servedDate),
    updated_round_month = round_date(servedDate, "month")
  ) %>%
  filter(updated_served_year != 2018) 


# Create a chart showing the number of first time visits over time
ggplot(visit, aes(x = round_month)) +
  geom_bar() +
  labs(title = "Number of First Time Pantry Visits (Monthly, Excluding 2018)",
       x = "Month", y = "Number of Visits (Grouped by Family ID)") + theme_minimal()

# Create a chart showing the number of first time visits over time with updated data
ggplot(visit_updated, aes(x = updated_round_month)) +
  geom_bar() +
  labs(title = "Number of First Time Pantry Visits (Monthly, Excluding 2018)",
       x = "Month", y = "Number of Visits (Grouped by Family ID)") + theme_minimal()

#----

#Objective: Forecasting What New visitors to the pantries 
# will Look Like in the future

#Time Series for Annual Income
#----
income_time_series <- visit %>%
  group_by(round_month) %>%
  summarise(
    avg_annual_income = mean(annual_income, na.rm = TRUE),
    median_annual_income = median(annual_income, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(round_month)

# View the time series
print(income_time_series)

ggplot(income_time_series, aes(x = round_month)) +
  geom_line(aes(y = avg_annual_income), color = "black") +
  geom_point(aes(y = avg_annual_income), color = "black") +
  geom_smooth(aes(y = avg_annual_income), method = "loess",
              color = "black", linetype = "dotted", se = FALSE)+
  labs(
    title = "Average Annual Income of Pantry Visitors",
    x = "Month",
    y = "Average Annual Income",
  ) +
  theme_minimal()


#----
#Forecasting 
#----

# Aggregate the data to get the number of new visitors per month
visit_monthly <- visit %>%
  group_by(round_month) %>%
  summarise(
    new_visits = n()  # Number of new visits (first-time visits) per month
  )

visit_monthly <- visit_monthly %>%
  mutate(
    month = month(round_month),
    year = year(round_month),
    quarter = quarter(round_month),
    day_of_week = weekdays(round_month)
  )

# Fit a linear regression model
lm_model <- lm(new_visits ~ month + year + quarter + day_of_week, data = visit_monthly)

# Predict the number of new visitors for future months (create a new dataset with future months)
future_data <- data.frame(
  month = c(1, 2, 3), 
  year = c(2025, 2025, 2025),
  quarter = c(1, 1, 1),
  day_of_week = c('Monday', 'Tuesday', 'Wednesday')
)

# Predict new visits for the future months
predictions <- predict(lm_model, newdata = future_data)

# Model performance metrics
predicted_values <- predict(lm_model)
actual_values <- visit_monthly$new_visits

# Calculate MSE
MSE <- mean((predicted_values - actual_values)^2)

# Check R-squared value
summary(lm_model)$r.squared

# Predict the number of new visitors for each month in 2024
future_data_2024 <- data.frame(
  month = rep(1:12, each = 1),  
  year = rep(2024, 12), 
  quarter = rep(1:4, length.out = 12),
  day_of_week = weekdays(as.Date(paste(2024, 1:12, 1, sep = "-")))  
)

# Predict new visits for the months of 2024
predictions_2024 <- predict(lm_model, newdata = future_data_2024)

predicted_2024_df <- data.frame(
  round_month = as.Date(paste(2024, future_data_2024$month, 1, sep = "-")),
  predicted_visits = predictions_2024,
  year = 2024,
  month = future_data_2024$month
)

#----
# Predict new visits for the months of 2024
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
  geom_line(data = visit_monthly, aes(x = round_month, y = new_visits), color = 'blue') +
  # Plot predicted data for 2024
  geom_line(data = predicted_2024_df, aes(x = round_month, y = predicted_visits), color = 'red', linetype = "dashed") +
  labs(title = "Observed and Predicted New Visitors", x = "Year", y = "Number of New Visitors") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  # Format X-axis to show only the year
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate month labels for better spacing if needed
    legend.title = element_blank()
  )
  
# Combine observed data and predicted data
observed_2024 <- visit_monthly %>%
  filter(year == 2024)  # Filter only the data for 2024
  
#Zoomed in Predictions for 2024
ggplot() +
  geom_line(data = observed_2024, aes(x = round_month, y = new_visits), color = 'blue', size = 1) +
  geom_line(data = predicted_2024_df, aes(x = round_month, y = predicted_visits), color = 'red', linetype = "dashed", size = 1) +
  labs(title = "Predicted New Visitors for 2024", x = "Month", y = "Number of New Visitors") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +  # Format X-axis to show month names
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate month labels for better spacing
    legend.title = element_blank()
  )

#----
#Forecasting pt2 (Seasonality with Sarima)
#----

# Aggregate the data to get the number of new visitors per month
visit_monthly <- visit %>%
  group_by(round_month) %>%
  summarise(
    new_visits = n()  # Number of new visits (first-time visits) per month
  )

# Convert round_month to Date class and extract relevant time variables
visit_monthly <- visit_monthly %>%
  mutate(
    round_month = as.Date(round_month),
    month = month(round_month),
    year = year(round_month),
    quarter = quarter(round_month),
    day_of_week = weekdays(round_month)
  )

# Create time series object for SARIMA model
ts_data <- ts(visit_monthly$new_visits, frequency = 12, start = c(min(visit_monthly$year), min(visit_monthly$month)))

# Fit SARIMA model (Seasonal ARIMA)
sarima_model <- auto.arima(ts_data, seasonal = TRUE)

# Forecast new visits for 2024 using the SARIMA model
sarima_forecast <- forecast(sarima_model, h = 12)

#----
# Create the future data for predictions 2024
#----
future_data_2024 <- data.frame(
  month = 1:12,
  year = 2024,
  quarter = rep(1:4, length.out = 12),
  day_of_week = weekdays(as.Date(paste(2024, 1:12, 1, sep = "-"))),
  round_month = as.Date(paste(2024, 1:12, 1, sep = "-"))
)

# Generate predicted values using the linear regression model
lm_model <- lm(new_visits ~ month + year + quarter + day_of_week, data = visit_monthly)
predictions_lm <- predict(lm_model, newdata = future_data_2024)

# Create data frame for predictions (SARIMA vs LM)
predicted_2024_df <- data.frame(
  round_month = as.Date(paste(2024, future_data_2024$month, 1, sep = "-")),
  predicted_visits_sarima = sarima_forecast$mean,
  predicted_visits_lm = predictions_lm
)

sarima_forecast$mean

#----
#Test accuracy of models
#----

#Create a graph with the new observed 2024 data along side the base prediction model,
# and Sarima seasonality model

#Aggregate Observed Visits

# Count number of new households (first visit) per month in 2024
observed_2024 <- visit_updated %>%
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
    title = "Observed and Predicted New Visitors in 2024",
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
# Plot of New Visitors per month for different years
#----
ggplot(data = visit_monthly, aes(x = month, y = new_visits, color = factor(year), group = year)) +
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


