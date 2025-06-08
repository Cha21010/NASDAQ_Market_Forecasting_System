# ----------------------------------------  M1 and M2 data cleaning ---------------------------------------------
library(dplyr)
library(readr)
library(lubridate)
library(zoo)

m1_data <- read.csv('Raw Data/m1_Money Supply Measures.csv')
m2_data <- read.csv('Raw Data/m2_Money Supply Measures.csv')

convert_weekly_to_daily <- function(data, date_column, value_column, date_format) {
  cat('Original dates:', head(data[[date_column]]), '\n')
  data[[date_column]] <- as.Date(data[[date_column]], format = date_format)
  cat('Converted dates:', head(data[[date_column]]), '\n')
  data <- data %>% filter(!is.na(data[[date_column]]))
  full_dates <- seq(min(data[[date_column]]), max(data[[date_column]]), by = 'day')
  daily_data <- data.frame(Date = full_dates) %>%
    left_join(data, by = setNames(date_column, 'Date')) %>%
    arrange(Date) %>%
    mutate({{value_column}} := zoo::na.locf(.[[value_column]], na.rm = FALSE))
  return(daily_data)
}

m1_daily <- convert_weekly_to_daily(m1_data, 'observation_date', 'WM1NS', '%Y/%m/%d')
m2_daily <- convert_weekly_to_daily(m2_data, 'observation_date', 'WM2NS', '%Y-%m-%d')

m1_daily <- m1_daily %>% filter(year(Date) != 2019)
m2_daily <- m2_daily %>% filter(year(Date) != 2019)

write.csv(m1_daily, 'Cleaned Data/m1_daily_data.csv', row.names = FALSE)
write.csv(m2_daily, 'Cleaned Data/m2_daily_data.csv', row.names = FALSE)


# ---------------------------------- Oil and gold price cleaning -------------------------------------------
oil_data <- read.csv('Raw Data/oil_price_WTI.csv')
gold_data <- read.csv('Raw Data/gold_price.csv')

oil_data <- oil_data %>%
  rename(Date = observation_date, oil_price = DCOILWTICO)
gold_data <- gold_data %>%
  rename(Date = time, gold_price = USD)

write.csv(oil_data, 'Cleaned Data/cleaned_oil_price.csv', row.names = FALSE)
write.csv(gold_data, 'Cleaned Data/cleaned_gold_price.csv', row.names = FALSE)


# ------------------------------- CPI, PPI, and IM/EX price index cleaning -----------------------------------
library(readxl)
library(tidyr)

transform_to_monthly <- function(file_path, sheet_name) {
  data <- read_excel(file_path, sheet = sheet_name, col_names = TRUE, .name_repair = 'minimal', skip = 10)
  data <- data[,1:13]
  colnames(data) <- c('Year', paste0('M', 1:12))
  
  monthly_data <- data %>%
    pivot_longer(cols = starts_with('M'),
                 names_to = 'Month',
                 values_to = 'Value') %>%
    mutate(
      Month = as.numeric(gsub('M', '', Month)),
      Year = as.numeric(Year),
      Time = paste0(Year, '.', sprintf('%02d', Month))  
    ) %>%
    select(Time, Year, Month, Value) %>%
    arrange(Year, Month)
  
  return(monthly_data)
}

cpi_data <- transform_to_monthly('Raw Data/CPI-U_1month.xlsx', sheet_name = 1)
imexpi_data <- transform_to_monthly('Raw Data/IM:EXPI_1month.xlsx', sheet_name = 1)
ppi_data <- transform_to_monthly('Raw Data/PPI_1month.xlsx', sheet_name = 1)

transform_monthly_to_daily <- function(data) {
  daily_data <- data %>%
    mutate(
      Year = as.integer(sub("\\..*", "", Time)),
      Month = as.integer(sub(".*\\.", "", Time)),
      Date = as.Date(paste0(Year, '-', sprintf('%02d', Month), '-01'))
    ) %>%
    select(Date, Value) %>%
    arrange(Date) %>%
    rowwise() %>%
    do(data.frame(
      Date = seq(.$Date, by = 'day', length.out = days_in_month(.$Date)),
      Value = rep(.$Value, days_in_month(.$Date))
    )) %>%
    ungroup()
  
  return(daily_data)
}

cpi_data_daily <- transform_monthly_to_daily(cpi_data) %>%
  filter(format(Date, '%Y') >= 2020 & format(Date, '%Y') <= 2024) %>%
  rename(CPI = Value)

imexpi_data_daily <- transform_monthly_to_daily(imexpi_data) %>%
  filter(format(Date, '%Y') >= 2020 & format(Date, '%Y') <= 2024) %>%
  rename(`IM/EX` = Value)

ppi_data_daily <- transform_monthly_to_daily(ppi_data) %>%
  filter(format(Date, '%Y') >= 2020 & format(Date, '%Y') <= 2024) %>%
  rename(PPI = Value)

write.csv(cpi_data_daily, 'Cleaned Data/CPI_daily_data.csv', row.names = FALSE)
write.csv(imexpi_data_daily, 'Cleaned Data/IMEXPI_daily_data.csv', row.names = FALSE)
write.csv(ppi_data_daily, 'Cleaned Data/PPI_daily_data.csv', row.names = FALSE)


# ----------------------------------- Unemployment Rate Cleaning ---------------------------------------
unrate_data <- read_excel('Raw Data/UNRATE.xlsx', sheet = 'Monthly') %>%
  mutate(observation_date = as.Date(observation_date, format = "%Y-%m-%d")) %>%
  filter(observation_date >= as.Date('2020-01-01') & observation_date <= as.Date('2024-12-31'))

unempl_to_daily <- function(data) {
  daily_data <- data %>%
    mutate(observation_date = as.Date(observation_date)) %>%
    filter(!is.na(observation_date)) %>%
    arrange(observation_date) %>%
    rowwise() %>%
    do(data.frame(
      Date = seq(.$observation_date, by = 'day', length.out = days_in_month(.$observation_date)),
      UNRATE = rep(.$UNRATE, days_in_month(.$observation_date))
    )) %>%
    ungroup()
  
  return(daily_data)
}

unrate_data_daily <- unempl_to_daily(unrate_data)
write.csv(unrate_data_daily, 'Cleaned Data/UNRATE_daily_data.csv', row.names = FALSE)


# ------------------------------------- Treasury Bond Yield ---------------------------------------------
bond_data <- read_csv('Raw Data/Treasury_Bond_Yield.csv', na = "ND")

bond_data <- bond_data %>%
  mutate(across(-1, as.numeric)) 

colnames(bond_data) <- c(
  "Date",
  "Yield_1M",
  "Yield_3M",
  "Yield_6M",
  "Yield_1Y",
  "Yield_2Y",
  "Yield_3Y",
  "Yield_5Y",
  "Yield_7Y",
  "Yield_10Y",
  "Yield_20Y",
  "Yield_30Y"
)

write.csv(bond_data, 'Cleaned Data/cleaned_treasury_bond_yield_data.csv', row.names = FALSE)
