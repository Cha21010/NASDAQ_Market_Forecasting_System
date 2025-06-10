library(dplyr)
library(readr)
library(caret)
library(glmnet)
library(psych)
library(FactoMineR)
library(factoextra)
library(lubridate)
library(Metrics)

# Load and clean full dataset
df <- read_csv("D:/Columbia University/5205/group project/full_data.csv") %>%
  na.omit() %>%
  mutate(Date = as.Date(Date)) %>%
  select(-text, -TM_UpOrDown, -finbert_label)

# Forecasting range
start_forecast <- as.Date("2024-06-01")
end_forecast   <- as.Date("2024-12-31")
forecast_dates <- seq.Date(start_forecast, end_forecast, by = "day")

# Initialize results dataframe
results <- data.frame(
  forecast_date = as.Date(character()),
  predicted_TM_Close = numeric(),
  actual_TM_Close = numeric(),
  stringsAsFactors = FALSE
)

# Rolling loop
window_start <- as.Date("2020-02-24")

for (forecast_date in forecast_dates) {
  
  # Define training window: from window_start to day before forecast_date
  train_data <- df %>%
    filter(Date >= window_start & Date < forecast_date)
  
  test_data <- df %>%
    filter(Date == forecast_date)
  
  # Skip if not enough data
  if (nrow(train_data) < 100 || nrow(test_data) == 0) {
    window_start <- window_start + 1
    next
  }
  
  # Step 1: Lasso variable selection
  x <- model.matrix(TM_Close ~ ., data = train_data %>% select(-Date))[, -1]
  y <- train_data$TM_Close
  
  cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "gaussian")
  best_lambda <- cv_lasso$lambda.min
  lasso_model <- glmnet(x, y, alpha = 1, family = "gaussian", lambda = best_lambda)
  
  # Get selected variable names from Lasso (excluding intercept)
  lasso_coef <- coef(lasso_model)
  selected_vars <- rownames(lasso_coef)[lasso_coef[, 1] != 0][-1]  # drop intercept
  
  # Skip if no variables selected
  if (length(selected_vars) == 0) {
    window_start <- window_start + 1
    next
  }
  
  # Step 2: Select variables and run PCA
  # Keep only those that exist in the dataset
  selected_vars <- intersect(selected_vars, names(train_data))
  
  train_lasso_input <- train_data %>% select(all_of(selected_vars))
  test_lasso_input  <- test_data %>% select(all_of(selected_vars))
  
  if (ncol(train_lasso_input) < 2) {
    window_start <- window_start + 1
    next
  }
  
  pca_model <- prcomp(train_lasso_input, center = TRUE, scale. = TRUE)
  n_pcs <- min(4, ncol(pca_model$x))  # use up to 4 PCs
  
  train_pcs <- as.data.frame(predict(pca_model, newdata = train_lasso_input)[, 1:n_pcs, drop = FALSE])
  test_pcs  <- as.data.frame(predict(pca_model, newdata = test_lasso_input)[, 1:n_pcs, drop = FALSE])
  
  train_pcs$TM_Close <- train_data$TM_Close
  
  # Step 3: Train linear regression
  lm_model <- train(TM_Close ~ .,
                    data = train_pcs,
                    method = "lm",
                    trControl = trainControl(method = "none"))
  
  # Step 4: Predict
  pred <- predict(lm_model, newdata = test_pcs)
  actual <- test_data$TM_Close
  
  # Step 5: Save results
  results <- rbind(results, data.frame(
    forecast_date = forecast_date,
    predicted_TM_Close = pred,
    actual_TM_Close = actual
  ))
  
  cat("Predicted", forecast_date, ": ", round(pred, 2), " | Actual: ", actual, "\n")
  
  # Slide window forward
  window_start <- window_start + 1
}

# Calculate RMSE and MAE
rmse_val <- rmse(results$actual_TM_Close, results$predicted_TM_Close)
mae_val  <- mae(results$actual_TM_Close, results$predicted_TM_Close)
cat("Final RMSE:", rmse_val, "\n")
cat("Final MAE:", mae_val, "\n")

# Plot predicted vs actual
library(ggplot2)
ggplot(results, aes(x = forecast_date)) +
  geom_line(aes(y = actual_TM_Close), color = "black", linetype = "dashed") +
  geom_line(aes(y = predicted_TM_Close), color = "blue") +
  labs(title = "Rolling Lasso + PCA Forecast: TM_Close",
       subtitle = paste("RMSE =", round(rmse_val, 4), "| MAE =", round(mae_val, 4)),
       x = "Date",
       y = "Closing Price") +
  theme_minimal()
