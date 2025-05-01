rm(list = ls())

library(tidyverse)
library(caret)
library(pROC)
library(glmnet)
library(randomForest)

source("Code/CleanData/CleanRentData.R")

# Visualize the Data ----

# Scatter plot
ggplot(zori_data, aes(x = zori, y = num_VISITS)) +
  geom_point(size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Zillow Rent (ZORI) and DMARC Visits",
    x = "Zillow Observed Rent Index (ZORI)",
    y = "Number of DMARC Visits"
  )

# Timeseries: Rent and Visits
ggplot(zori_data, aes(x = floor_month)) +
  geom_line(aes(y = zori), color = "darkorange", linewidth = 1, linetype = "solid") +
  geom_line(aes(y = num_VISITS / 10), color = "steelblue", linewidth = 1, linetype = "dashed") +  # scaled for visibility
  labs(
    title = "ZORI vs DMARC Visits Over Time",
    x = "Month",
    y = "ZORI (Orange) / Visits ÷10 (Blue)"
  ) +
  theme_minimal()


# Model ----

lm_model <- lm(num_VISITS ~ zori, data = zori_data)
summary(lm_model)

zori_data <- zori_data %>%
  mutate(pred_visits = predict(lm_model))

ggplot(zori_data, aes(x = zori)) +
  geom_point(aes(y = num_VISITS), color = "blue") +
  geom_line(aes(y = pred_visits), color = "red", linewidth = 1.2) +
  labs(
    title = "Predicted vs Actual Visits by Rent",
    x = "ZORI",
    y = "Number of Visits"
  )






