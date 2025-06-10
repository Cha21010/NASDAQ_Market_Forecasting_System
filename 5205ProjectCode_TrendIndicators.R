library(dplyr)
library(readr)

# Load articles_by_date.csv
articles <- read_csv("Cleaned Data/articles_by_date.csv")
# Ensure text ends with a period
articles <- articles %>%
  mutate(text = ifelse(grepl("\\.$", text), text, paste0(text, ".")))
# Combine texts by date
articles <- articles %>%
  group_by(date) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  ungroup()
articles <- articles %>%
  rename(Date = date)
# Load 
gold_price <- read_csv("Cleaned Data/cleaned_gold_price.csv")
oil_price <- read_csv("Cleaned Data/cleaned_oil_price.csv")
oil_price <- oil_price %>%
  mutate(Date = as.Date(Date, format = "%Y/%m/%d"))
gold_price <- gold_price %>%
  mutate(Date = as.Date(Date, format = "%Y/%m/%d"))
treasury_yield <- read_csv("Cleaned Data/cleaned_treasury_bond_yield_data.csv")
cpi <- read_csv("Cleaned Data/CPI_daily_data.csv")
imexpi <- read_csv("Cleaned Data/IMEXPI_daily_data.csv")
m1 <- read_csv("Cleaned Data/m1_daily_data.csv")
m2 <- read_csv("Cleaned Data/m2_daily_data.csv")
nasdaq <- read_csv("Cleaned Data/NASDAQ_20-24.csv")
ppi <- read_csv("Cleaned Data/PPI_daily_data.csv")
unrate <- read_csv("Cleaned Data/UNRATE_daily_data.csv")

# Combine all the dataframes 
combined_df <- nasdaq
combined_df <- combined_df %>%
  left_join(articles, by = "Date") %>%
  left_join(gold_price, by = "Date") %>%
  left_join(oil_price, by = "Date") %>%
  left_join(treasury_yield, by = "Date") %>%
  left_join(cpi, by = "Date") %>%
  left_join(imexpi, by = "Date") %>%
  left_join(m1, by = "Date") %>%
  left_join(m2, by = "Date") %>%
  left_join(ppi, by = "Date") %>%
  left_join(unrate, by = "Date")
print(head(combined_df))
write_csv(combined_df, "full_combined_data.csv")

ibrary(dplyr)
library(readr)

# Load data
data <- read_csv("full_combined_data.csv")
str(data)

### ------------------------------ Add Financial Indicators Based on Market Data ----------------------------------
# 1. Trend Indicators:
# EMA (Exponential Moving Average)
#install.packages("TTR")
library(TTR)
# Calculate 5-day EMA
data$EMA_5 <- EMA(data$Close, n = 5)
# View the first few rows
head(data[, c("Date", "Close", "EMA_5")], 10)
# Calculate 10-day EMA
data$EMA_10 <- EMA(data$Close, n = 10)
# View the first few rows
head(data[, c("Date", "Close", "EMA_10")], 20)


# MACD (Moving Average Convergence Divergence)
macd_data <- MACD(data$Close, nFast = 12, nSlow = 26, nSig = 9, maType = EMA)
# Add MACD, Signal, and Histogram to the original data frame
data$MACD <- macd_data[, "macd"]
data$MACD_Signal <- macd_data[, "signal"]
data$MACD_Histogram <- data$MACD - data$MACD_Signal
print(data[, c("Date", "Close", "MACD", "MACD_Signal", "MACD_Histogram")], n=50)

# Bollinger Bands
bbands <- BBands(data$Close, n = 20, sd = 2)# Default parameters: n = 20 (20-day period), k = 2 (standard deviations)
bbands <- as.data.frame(bbands)
data$BB_Lower <- bbands$dn
data$BB_Middle <- bbands$mavg
data$BB_Upper <- bbands$up
print(data[, c("Date", "Close", "BB_Lower", "BB_Middle", "BB_Upper")], n=50)
# plot the Bollinger Bands Data
plot(data$Date, data$Close, type = "l", col = "black", lwd = 1.5,
     main = "Bollinger Bands", xlab = "Date", ylab = "Price")
lines(data$Date, data$BB_Upper, col = "blue", lwd = 1.2)
lines(data$Date, data$BB_Middle, col = "gray", lty = 2)
lines(data$Date, data$BB_Lower, col = "blue", lwd = 1.2)
legend("topright", legend = c("Price", "Upper", "Middle", "Lower"),
       col = c("black", "blue", "gray", "blue"), lty = c(1,1,2,1))

# ADX (Average Directional Index)
adx_data <- ADX(HLC = data[, c("High", "Low", "Close")], n = 14)
adx_data <- as.data.frame(adx_data)
data$DIp <- adx_data$DIp
data$DIn <- adx_data$DIn
data$ADX <- adx_data$ADX
print(data[, c("Date", "Close", "DIp", "DIn", "ADX")], n=50)

# --------------------- 2. Momentum Indicators ------------------------
# RSI (Relative Strength Index)
data$RSI <- RSI(data$Close, n = 14)
# CCI (Commodity Channel Index)
data$CCI <- CCI(data[, c("High", "Low", "Close")], n = 20)
# MoneyFlowMultiplier
data$MoneyFlowMultiplier <- ((data$Close - data$Low) - (data$High - data$Close)) / (data$High - data$Low)
data$MoneyFlowMultiplier[is.nan(data$MoneyFlowMultiplier)] <- 0
# MoneyFlowVolume
data$MoneyFlowVolume <- data$MoneyFlowMultiplier * data$Volume
# CMF (Chaikin Money Flow)
data$CMF <- CMF(HLC = data[, c("High", "Low", "Close")], volume = data$Volume, n = 20)
# Stochastic Oscillator
stoch_data <- stoch(data[, c("High", "Low", "Close")])
data$Stoch_K <- stoch_data[, "fastK"]
data$Stoch_D <- stoch_data[, "fastD"]
print(data[, c("RSI", "CCI", "CMF", "Stoch_K", "Stoch_D")], n=50)

# -------------------- 3. Volatility Indicators ------------------------
# TR (True Range)
tr_data <- as.data.frame(TR(data[, c("High", "Low", "Close")]))
data$tr <- tr_data$tr

# ATR (Average True Range)
atr_data <- as.data.frame(ATR(data[, c("High", "Low", "Close")], n = 14))
data$atr <- atr_data$atr
# Log Returns
data$log_return <- c(NA, diff(log(data$Close)))

# Chaikin_Volatility
# Calculate high-low range
hl_range <- data$High - data$Low
# Compute EMAs
ema_3 <- EMA(hl_range, n = 3)
ema_10 <- EMA(hl_range, n = 10)
# Calculate Chaikin Volatility (as percentage change)
data$Chaikin_Volatility <- 100 * (ema_3 - ema_10) / ema_10

# -------------------- 4. Volume Indicators ---------------------------
# Chaikin Accumulation/Distribution Line (AD)
data$Chaikin_AD <- chaikinAD(HLC = data[, c("High", "Low", "Close")], volume = data$Volume)
# OBV (On-Balance Volume)
data$OBV <- OBV(data$Close, data$Volume)
# VWAP (Volume Weighted Average Price)
data$VWAP <- cumsum(data$Close * data$Volume) / cumsum(data$Volume)

# ----------------------- 5. Lagging Features --------------------------
# 7-day moving averages
data$MA7_Close <- rollmean(data$Close, 7, fill = NA, align = "right")
# 15-day moving averages
data$MA15_Close <- rollmean(data$Close, 15, fill = NA, align = "right")

# --------------------- 6. Prediction-Related Features -------------------
# Tomorrow's close and open prices
data$TM_Close <- lead(data$Close, 1)
# Tomorrow's direction (Up or Down)
data$TM_UpOrDown <- ifelse(lead(data$Close, 1) > data$Close, "Up", "Down")

# --------------------- Save the final enriched dataset ------------------
write_csv(data, "full_data_1.csv")

