rm(list = ls())

library(caret)
library(pROC)
library(glmnet)
library(randomForest)

source("Code/CleanData/CleanCPI.R")
source("Code/CleanData/CleanRentData.R")

# Merging CPI and Rent data
cpi_rent_combined <- monthly_and_cpi %>%
  left_join(zori_polk_long, by = "floor_month")

# Random Forest Model ----
# Set seed and RNG settings
RNGkind(sample.kind = "default")
set.seed(2291352)

# Select modeling data
rf_data <- cpi_rent_combined %>%
  select(num_VISITS, Food, Meat, FruitsVeggies, CerealBakery, Dairy, zori) %>%
  drop_na()

# Train-test split
train.idx <- sample(1:nrow(rf_data), size = floor(0.8 * nrow(rf_data)))
train.df <- rf_data[train.idx, ]
test.df  <- rf_data[-train.idx, ]

# Initial forest model
myforest <- randomForest(num_VISITS ~ ., 
                         data = train.df,
                         ntree = 1000,
                         mtry = 3,  # sqrt of 6 predictors rounded up
                         importance = TRUE)

print(myforest)

# Try different mtry values
mtry <- 1:6
keeps <- data.frame(m = mtry, OOB_MSE = NA)

for (i in seq_along(mtry)) {
  tempforest <- randomForest(num_VISITS ~ ., data = train.df,
                             ntree = 1000,
                             mtry = mtry[i])
  keeps$OOB_MSE[i] <- mean((predict(tempforest) - train.df$num_VISITS)^2)
}

# Plot OOB MSE
ggplot(keeps, aes(x = m, y = OOB_MSE)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "m (mtry) value", y = "OOB MSE",
       caption = "Lower is better — choose m with lowest OOB error")

# Final model with best mtry
best_m <- keeps$m[which.min(keeps$OOB_MSE)]

final_forest <- randomForest(num_VISITS ~ ., 
                             data = train.df,
                             ntree = 1000,
                             mtry = best_m,
                             importance = TRUE)

# Predict on test set
test.df$pred_visits <- predict(final_forest, newdata = test.df)

# Evaluation metrics
rmse <- sqrt(mean((test.df$pred_visits - test.df$num_VISITS)^2))
r_squared <- cor(test.df$pred_visits, test.df$num_VISITS)^2

cat("Test RMSE:", round(rmse, 2), "\n")
cat("Test R²:", round(r_squared, 3), "\n")

# Variable importance
varImpPlot(final_forest, type = 1)

# Actual vs Predicted Plot
ggplot(test.df, aes(x = num_VISITS, y = pred_visits)) +
  geom_point(color = "darkblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Visits",
       x = "Actual Visits",
       y = "Predicted Visits") +
  theme_minimal()

mean_visits <- mean(test.df$num_VISITS)
relative_rmse <- rmse / mean_visits

cat("Mean Visits:", round(mean_visits, 2), "\n")
cat("Relative RMSE:", round(relative_rmse * 100, 2), "%\n")


