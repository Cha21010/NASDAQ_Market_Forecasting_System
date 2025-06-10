library(dplyr)
library(readr)
library(xts)
library(forecast)
library(tseries)
library(lubridate)
library(ggplot2)
library(Metrics)  # for rmse()

# Load and sort the data
df <- read_csv("D:/Columbia University/5205/group project/full_data.csv")
df <- df %>% arrange(as.Date(Date))

# Set forecast range
start_forecast <- as.Date("2024-06-02")
end_forecast <- as.Date("2024-12-31")

# Initialize prediction results
results <- data.frame(
  forecast_date = as.Date(character()),
  predicted_close = numeric(),
  stringsAsFactors = FALSE
)

# Initialize sliding window start date
window_start <- as.Date("2020-02-25")

# Loop through each forecast date
for (forecast_date in seq.Date(start_forecast, end_forecast, by = "day")) {
  
  # Define training window: from window_start to the day before forecast_date
  window_end <- forecast_date - 1
  
  # Get training data
  train_data <- df %>%
    filter(as.Date(Date) >= window_start & as.Date(Date) <= window_end) %>%
    select(Date, Close) %>%
    na.omit()
  
  # Skip if training data is too short
  if (nrow(train_data) < 30) next
  
  # Convert to time series object
  price_xts <- xts(train_data$Close, order.by = as.Date(train_data$Date))
  
  # Difference the series to make it stationary
  diff_price_xts <- diff(price_xts)
  diff_price_xts <- na.omit(diff_price_xts)
  
  # Fit ARIMA model on differenced series
  model_arima <- auto.arima(diff_price_xts)
  
  # Forecast the next differenced value
  forecast_diff <- forecast(model_arima, h = 1)
  predicted_change <- as.numeric(forecast_diff$mean)
  
  # Recover the predicted closing price by adding change to the last known price
  last_close <- as.numeric(last(price_xts))
  predicted_close <- last_close + predicted_change
  
  # Save the prediction
  results <- rbind(results, data.frame(
    forecast_date = forecast_date,
    predicted_close = predicted_close
  ))
  
  cat("Forecast date:", forecast_date, "Predicted close:", predicted_close, "\n")
  
  # Slide the window one day forward
  window_start <- window_start + 1
}

# Preview results
head(results)

# Join with actual closing prices for RMSE calculation
actuals <- df %>%
  filter(as.Date(Date) >= start_forecast & as.Date(Date) <= end_forecast) %>%
  select(Date, Close) %>%
  rename(forecast_date = Date, actual_close = Close)

# Merge predictions with actuals
results$forecast_date <- as.Date(results$forecast_date)
actuals$forecast_date <- as.Date(actuals$forecast_date)
results_with_actuals <- left_join(results, actuals, by = "forecast_date")

# Compute RMSE (remove any NA in actuals)
rmse_score <- sqrt(mean((results_with_actuals$actual_close - results_with_actuals$predicted_close)^2, na.rm = TRUE))

cat("RMSE for the forecast period (2024-06-02 to 2024-12-31):", rmse_score, "\n")

# Plot predicted vs actual
ggplot(results_with_actuals, aes(x = forecast_date)) +
  geom_line(aes(y = actual_close), color = "black", linetype = "dashed") +
  geom_line(aes(y = predicted_close), color = "blue") +
  labs(title = "Predicted vs Actual Closing Prices",
       x = "Date",
       y = "Closing Price",
       caption = paste("RMSE =", round(rmse_score, 4))) +
  theme_minimal()
