rm(list = ls())

library(tidyverse)
library(caret)
library(pROC)
library(glmnet)
library(randomForest)

source("Code/CleanData/CleanCPI.R")


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
# I used ChatGPT to help produce this visual since 
# Visits and CPI are on different scales/metrics
ggplot(monthly_and_cpi, aes(x = floor_month)) +
  geom_line(aes(y = num_VISITS), color = "steelblue", linewidth = 1.2) +
  geom_line(aes(y = Food * 30), color = "firebrick", linetype = "dashed", linewidth = 1.2) +  # scale CPI for visibility
  scale_y_continuous(
    name = "Number of Visits",
    sec.axis = sec_axis(~ . / 30, name = "Food CPI")  # rescale right axis back to original CPI
  ) +
  labs(
    title = "DMARC Visits and Food CPI Over Time",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "steelblue"),
    axis.text.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "firebrick"),
    axis.text.y.right = element_text(color = "firebrick")
  )

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
lm_model_lag <- lm(num_VISITS ~ Meat_lag1 + Food_lag1 + CerealBakery_lag1 +
                     CerealBakery + Dairy, data = monthly_and_cpi)
summary(lm_model_lag)

# Try hypothetical CPI values
new_cpi_scenario <- data.frame(
  Food = 330,
  Meat = 350,
  FruitsVeggies = 355,
  CerealBakery = 360,
  Dairy = 275,
  month = factor("Jan", levels = levels(monthly_and_cpi$month))  # or any other month like "Feb", "Mar", etc.
)

# Hypothetical Situations
# Predict number of visits using lm model with seasonality
predicted_visits <- predict(lm_model_time, newdata = new_cpi_scenario)
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
# I've only done Random Forest classification in R

# RMSE and R-squared
rmse <- sqrt(mean((test.df$pred_visits - test.df$num_VISITS)^2))
r_squared <- cor(test.df$pred_visits, test.df$num_VISITS)^2

cat("Test RMSE:", round(rmse, 2), "\n")
cat("Test R²:", round(r_squared, 3), "\n")

mean_visits <- mean(test.df$num_VISITS)
relative_rmse <- rmse / mean_visits

cat("Mean Visits:", round(mean_visits, 2), "\n")
cat("Relative RMSE:", round(relative_rmse * 100, 2), "%\n")

# Plot variable importance
varImpPlot(final_forest, type = 1)


ggplot(test.df, aes(x = num_VISITS, y = pred_visits)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Visits",
       x = "Actual Visits",
       y = "Predicted Visits") +
  theme_minimal()


# Predicting based on hypothetical situations
# Inserting different potential values

cpi_scenario <- data.frame(
  Food = 330,
  Meat = 350,
  FruitsVeggies = 355,
  CerealBakery = 360,
  Dairy = 275
)

# Predict number of visits
predicted_visits <- predict(final_forest, newdata = cpi_scenario)
print(predicted_visits)



