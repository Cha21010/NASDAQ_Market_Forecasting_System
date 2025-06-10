library(dplyr)
library(readr)
library(xgboost)
library(vtreat)
library(lubridate)
library(Metrics)
library(ggplot2)

# Load the dataset
df <- read_csv("D:/Columbia University/5205/group project/full_data.csv") %>%
  na.omit() %>%
  mutate(Date = as.Date(Date)) %>%
  select(-text, -TM_UpOrDown, -finbert_label)

# Define forecast period
start_forecast <- as.Date("2024-06-02")
end_forecast <- as.Date("2024-12-31")
date_seq <- seq.Date(start_forecast, end_forecast, by = "day")

# Initialize results data frame
xgb_results <- data.frame(
  forecast_date = as.Date(character()),
  predicted_TM_Close = numeric(),
  actual_TM_Close = numeric(),
  stringsAsFactors = FALSE
)

# Initialize sliding window start
window_start <- as.Date("2020-02-25")

# Loop through each forecast date
for (forecast_date in date_seq) {
  
  # Define training window: from window_start to forecast_date - 1
  train_start <- window_start
  train_end <- forecast_date - 1
  
  # Filter training and testing data
  train_data <- df %>% filter(Date >= train_start & Date <= train_end)
  test_data <- df %>% filter(Date == forecast_date)
  
  # Skip if insufficient training data or no test data
  if (nrow(train_data) < 100 || nrow(test_data) == 0) {
    window_start <- window_start + 1  # Slide window forward
    next
  }
  
  # Design treatment plan
  trt <- designTreatmentsZ(
    dframe = train_data,
    varlist = setdiff(names(train_data), c("Date", "TM_Close"))
  )
  newvars <- trt$scoreFrame %>% filter(code %in% c('clean', 'lev')) %>% pull(varName)
  
  train_input <- prepare(trt, train_data, varRestriction = newvars)
  test_input <- prepare(trt, test_data, varRestriction = newvars)
  
  train_matrix <- xgb.DMatrix(data = as.matrix(train_input), label = train_data$TM_Close)
  test_matrix <- xgb.DMatrix(data = as.matrix(test_input))
  
  # Train XGBoost model
  xgb_model <- xgb.train(
    params = list(
      objective = "reg:squarederror",
      eta = 0.05,
      max_depth = 4,
      subsample = 0.7,
      colsample_bytree = 0.7,
      lambda = 2,
      alpha = 1,
      eval_metric = "rmse"
    ),
    data = train_matrix,
    nrounds = 200,
    verbose = 0
  )
  
  # Predict
  pred <- predict(xgb_model, newdata = test_matrix)
  
  # Save result
  xgb_results <- rbind(xgb_results, data.frame(
    forecast_date = forecast_date,
    predicted_TM_Close = pred,
    actual_TM_Close = test_data$TM_Close
  ))
  
  # Print status
  cat("Forecast for", forecast_date, "-> Predicted:", round(pred, 2), "Actual:", test_data$TM_Close, "\n")
  
  # Slide window forward by one day
  window_start <- window_start + 1
}

# Compute RMSE and MAE
xgb_rmse <- rmse(xgb_results$actual_TM_Close, xgb_results$predicted_TM_Close)
xgb_mae <- mae(xgb_results$actual_TM_Close, xgb_results$predicted_TM_Close)
cat("Sliding Window XGBoost RMSE:", xgb_rmse, "\n")
cat("Sliding Window XGBoost MAE:", xgb_mae, "\n")

# Plot predicted vs actual
ggplot(xgb_results, aes(x = forecast_date)) +
  geom_line(aes(y = actual_TM_Close), color = "black", linetype = "dashed") +
  geom_line(aes(y = predicted_TM_Close), color = "blue") +
  labs(
    title = "Sliding Window XGBoost Forecast: TM_Close",
    x = "Date",
    y = "Closing Price",
    caption = paste("RMSE:", round(xgb_rmse, 4), "| MAE:", round(xgb_mae, 4))
  ) +
  theme_minimal()
