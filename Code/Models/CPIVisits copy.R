rm(list = ls())

library(tidyverse)
library(caret)
library(pROC)
library(glmnet)
library(randomForest)

source("Code/CleanCPI.R")


# Visualize -------

# Plot number of visits over time
ggplot(monthly_and_cpi, aes(x = floor_month)) +
  geom_line(aes(y = num_VISITS), color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Monthly Visits to DMARC Food Pantries",
    x = "Month",
    y = "Number of Visits"
  ) +
  theme_minimal()

# Plot Food CPI over time
ggplot(monthly_and_cpi, aes(x = floor_month)) +
  geom_line(aes(y = Food), color = "firebrick", linewidth = 1.2) +
  labs(
    title = "Food CPI Over Time",
    x = "Month",
    y = "Food CPI"
  ) +
  theme_minimal()

# Compare Visits and Food CPI together
ggplot(monthly_and_cpi, aes(x = floor_month)) +
  geom_line(aes(y = num_VISITS), color = "steelblue", linewidth = 1.2) +
  geom_line(aes(y = Food * 10), color = "firebrick", linewidth = 1.2, linetype = "dashed") +  # scaled CPI
  scale_y_continuous(
    name = "Number of Visits",
    sec.axis = sec_axis(~./10, name = "Food CPI (scaled)")
  ) +
  labs(
    title = "Food Pantry Visits vs. Food Prices",
    x = "Month"
  ) +
  theme_minimal() + 
  geom_smooth(aes(y = num_VISITS), method = "loess", se = FALSE, color = "navy")

# Look for correlation -----

# Calculate correlation between Food CPI and number of visits
cor_test_result <- cor.test(monthly_and_cpi$num_VISITS, monthly_and_cpi$Food)

print(cor_test_result)

cor_test_result <- cor.test(monthly_and_cpi$num_VISITS, monthly_and_cpi$FruitsVeggies)

print(cor_test_result)

cor_test_result <- cor.test(monthly_and_cpi$num_VISITS, monthly_and_cpi$CerealBakery)

print(cor_test_result)

cor_test_result <- cor.test(monthly_and_cpi$num_VISITS, monthly_and_cpi$Dairy)

print(cor_test_result)

ggplot(monthly_and_cpi, aes(x = Food, y = num_VISITS)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    title = "Relationship Between Food CPI and Pantry Visits",
    x = "Food CPI",
    y = "Number of Visits"
  ) +
  theme_minimal()

# Linear regression model ----
lm_model <- lm(num_VISITS ~ Food + Meat + FruitsVeggies + CerealBakery + Dairy, data = monthly_and_cpi)

# Model summary
summary(lm_model)

# Model with time included
lm_model_time <- lm(num_VISITS ~ Food + Meat + FruitsVeggies + CerealBakery + Dairy + factor(month), data = monthly_and_cpi)
summary(lm_model_time)

# Model with lag included
lm_model_lag <- lm(num_VISITS ~ Meat_lag1 + Food_lag1 + Cereal_lag1 +
                     CerealBakery + Dairy, data = monthly_and_cpi)
summary(lm_model_lag)

# Try hypothetical CPI values
new_cpi_scenario <- data.frame(
  Food = 320,
  Meat = 310,
  FruitsVeggies = 290,
  CerealBakery = 280,
  Dairy = 272
)

# Predict number of visits
predicted_visits <- predict(lm_model, newdata = new_cpi_scenario)
print(predicted_visits)


# Random Forest Regression ----

# Set seed and use default RNG settings
RNGkind(sample.kind = "default")
set.seed(2291352)

# Use your CPI + visit dataset
cdata <- monthly_and_cpi %>%
  select(num_VISITS, Food, Meat, FruitsVeggies, CerealBakery, Dairy) %>%
  drop_na()

# Train-test split
train.idx <- sample(1:nrow(cdata), size = floor(0.8 * nrow(cdata)))
train.df <- cdata[train.idx, ]
test.df  <- cdata[-train.idx, ]

# Initial forest
myforest <- randomForest(num_VISITS ~ ., 
                         data = train.df,
                         ntree = 1000,
                         mtry = 3,  # sqrt of num features (6), rounded up
                         importance = TRUE)

myforest

# Try different m values (1 to number of predictors)
mtry <- 1:5

keeps <- data.frame(m = rep(NA, length(mtry)),
                    OOB_MSE = rep(NA, length(mtry)))

for (idx in 1:length(mtry)) {
  tempforest <- randomForest(num_VISITS ~ ., data = train.df,
                             ntree = 1000,
                             mtry = mtry[idx])
  keeps[idx, "m"] <- mtry[idx]
  keeps[idx, "OOB_MSE"] <- mean((predict(tempforest) - train.df$num_VISITS)^2)
}

ggplot(data = keeps, aes(m, OOB_MSE)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "m (mtry) value", y = "OOB MSE",
       caption = "Lower is better — choose m with lowest OOB error")

# Use the m value with lowest OOB MSE
best_m <- keeps$m[which.min(keeps$OOB_MSE)]

final_forest <- randomForest(num_VISITS ~ ., 
                             data = train.df,
                             ntree = 1000,
                             mtry = best_m,
                             importance = TRUE)
# Predict on test set
test.df$pred_visits <- predict(final_forest, test.df)

# I used ChatGPT here to help evaluate the model
# I've only done classification in R

# RMSE and R-squared
rmse <- sqrt(mean((test.df$pred_visits - test.df$num_VISITS)^2))
r_squared <- cor(test.df$pred_visits, test.df$num_VISITS)^2

cat("Test RMSE:", round(rmse, 2), "\n")
cat("Test R²:", round(r_squared, 3), "\n")

# Plot variable importance
varImpPlot(final_forest, type = 1)


ggplot(test.df, aes(x = num_VISITS, y = pred_visits)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Visits",
       x = "Actual Visits",
       y = "Predicted Visits") +
  theme_minimal()






