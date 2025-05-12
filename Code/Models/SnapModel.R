# ================================
# SNAP Participation Analysis
# ================================

# Clear environment and load data
rm(list = ls())
source("Code/Visit Level Analysis.R")  # Load cleaned Red Barrel data

# --------------------------------
# Load libraries
# --------------------------------
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)

# --------------------------------
# Variable Transformations (Histogram Checks)
# --------------------------------

# Annual Income
par(mfrow = c(2, 3))
currVar <- visit$annual_income
hist(currVar)
hist(log(currVar))
hist(log10(currVar))        # Best transformation
hist(sqrt(currVar))
hist(1 / currVar)
hist(-1 / currVar)

# Federal Poverty Level
par(mfrow = c(2, 3))
currVar <- visit$fed_poverty_level
hist(currVar)
hist(log(currVar))          # Best transformation
hist(log10(currVar))
hist(sqrt(currVar))
hist(1 / currVar)
hist(-1 / currVar)

# Household Size
par(mfrow = c(2, 3))
currVar <- visit$n_household
hist(currVar)               # Best transformation
hist(log(currVar))
hist(log10(currVar))
hist(sqrt(currVar))
hist(1 / currVar)
hist(-1 / currVar)

# --------------------------------
# Feature Engineering
# --------------------------------

visit <- visit %>%
  mutate(
    snap_household = factor(snap_household, levels = c("N", "Y")),
    served_date = as.Date(served_date),
    served_year = year(served_date),
    served_month = month(served_date)
  )

# Select relevant features
model_data <- visit %>%
  select(
    snap_household,
    annual_income,
    fed_poverty_level,
    served_year,
    served_month,
    n_household,
    gender,
    race,
    family_type,
    education
  )

# Standardize numeric features
numeric_vars <- model_data %>%
  select(annual_income, fed_poverty_level, served_year, served_month, n_household)

model_data[names(numeric_vars)] <- scale(numeric_vars)

# Convert character variables to factors
model_data <- model_data %>%
  mutate(
    gender = as.factor(gender),
    race = as.factor(race),
    family_type = as.factor(family_type),
    education = as.factor(education)
  )

# --------------------------------
# Train/Test Split
# --------------------------------

set.seed(123)
train_index <- createDataPartition(model_data$snap_household, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# --------------------------------
# Model Fitting and Evaluation
# --------------------------------

# Logistic Regression
model <- glm(snap_household ~ ., data = train_data, family = "binomial")

# Predictions
pred_probs <- predict(model, newdata = test_data, type = "response")
pred_classes <- factor(ifelse(pred_probs > 0.5, "Y", "N"), levels = c("N", "Y"))

# Confusion Matrix
conf_mat <- confusionMatrix(pred_classes, test_data$snap_household)
print(conf_mat)

# Add predicted class for later use
test_data$pred_class <- ifelse(pred_probs > 0.5, "Likely SNAP User", "Unlikely SNAP User")

# Preview predictions
head(test_data[, c("snap_household", "pred_class")])

# --------------------------------
# Visualization
# --------------------------------

# SNAP by Household Size
ggplot(data = visit) +
  geom_bar(aes(x = n_household, fill = snap_household), position = "fill") +
  labs(
    x = "Number of People in Household",
    y = "Proportion",
    title = "SNAP Participation by Household Size"
  ) +
  scale_fill_grey(name = "SNAP\nParticipation", start = 0.3, end = 0.7) +
  theme_minimal()

# Bin income and visualize
visit <- visit %>%
  mutate(
    income_bracket = cut(
      annual_income,
      breaks = c(-Inf, 10000, 25000, 50000, 75000, 100000, Inf),
      labels = c("<10k", "10-25k", "25-50k", "50-75k", "75-100k", "100k+"),
      right = FALSE
    )
  )

ggplot(data = visit) +
  geom_bar(aes(x = income_bracket, fill = snap_household), position = "fill") +
  labs(
    x = "Annual Household Income",
    y = "Proportion",
    title = "SNAP Participation by Income Bracket"
  ) +
  scale_fill_grey(name = "SNAP\nParticipation") +
  theme_minimal()

# --------------------------------
# N_Household vs SNAP Pred
# --------------------------------

# Add unscaled household size back to test_data
test_data$unscaled_n_household <- visit$n_household[-train_index]

# Plot using unscaled household size
ggplot(data = test_data) +
  geom_bar(aes(x = unscaled_n_household, fill = pred_class), position = "fill") +
  labs(
    x = "Number of People in Household",
    y = "Proportion",
    title = "Predicted SNAP Participation by Household Size"
  ) +
  scale_fill_grey(name = "Predicted SNAP\nUsage") +
  theme_minimal()

# Calculate odds
odds_data <- test_data %>%
  group_by(unscaled_n_household, pred_class) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = pred_class, values_from = count, values_fill = 0) %>%
  mutate(
    odds = (`Likely SNAP User`) / (`Unlikely SNAP User`)
  ) %>%
  rename(n_household = unscaled_n_household)

print(odds_data)

