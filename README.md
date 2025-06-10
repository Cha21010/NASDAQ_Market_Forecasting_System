# NASDAQ Market Forecasting Using Machine Learning  
*APAN5205 Final Project - Group 4*

## 📘 Overview

This project explores predictive modeling of the NASDAQ Composite Index using financial, macroeconomic, and sentiment data from 2020 to 2024. The goal is to compare the forecasting accuracy of three models—XGBoost, PCA + Lasso Regression, and ARIMA—under a rolling daily prediction scheme.

---

## 📊 Data Pipeline

### Data Sources
- **NASDAQ Market Data (2020–2024)**: From Yahoo Finance  
- **Macroeconomic Indicators**: UNRATE, CPI, PPI, IM/EX, M1, M2 from BLS & FRED  
- **Commodity Prices**: Oil (WTI), Gold from FRED  
- **Treasury Yields**: 1M to 30Y from BEA  
- **News Articles**: Financial news from The Guardian API  
- **Sentiment Analysis**: Using FinBERT (financial NLP model)

### Feature Engineering

Data was enriched with multiple technical indicators:
- **Trend**: EMA, MACD, Bollinger Bands, ADX  
- **Momentum**: RSI, CCI, CMF  
- **Volatility**: ATR, True Range (TR), Log Returns, Chaikin Volatility  
- **Volume**: OBV, VWAP, Chaikin A/D  
- **Lagging Averages**: 7-day & 15-day rolling means  
- **Sentiment**: FinBERT-derived label & score from article text  

The final dataset includes over **60 engineered features**, cleaned and aligned by date, with a target variable `TM_Close` (tomorrow’s closing price).

---

## 🧠 Models Used

### 1. **XGBoost**
- Tree-based ensemble model
- Used `vtreat` for feature engineering
- Hyperparameter tuning with cross-validation
- Strong in-sample performance, but signs of overfitting out-of-sample

### 2. **PCA + Lasso Regression**
- Dimensionality reduction with PCA
- Variable selection using Lasso
- Robust and interpretable model
- Balanced tradeoff between accuracy and generalization

### 3. **ARIMA**
- Classical time-series model
- First-differencing applied for stationarity
- Parameters selected via `auto.arima()`
- Most accurate model during 2024 out-of-sample phase

---

## 🔄 Rolling Forecast Strategy

Every model used a **rolling one-day-ahead prediction** method:
1. Train on all available historical data
2. Predict the next day’s closing price
3. Add new actual data and repeat
4. Continue from June to December 2024

---

## 📈 Results Summary

| Model         | RMSE (Out-of-sample) | MAE (Out-of-sample) |
|---------------|----------------------|----------------------|
| ARIMA         | **221.46**           | —                    |
| PCA + Lasso   | 229.07               | **168.21**           |
| XGBoost       | 305.47               | 256.88               |

---

## 📌 File Structure

- `full_data.csv`: Final feature-enriched dataset  
- `full_data_1.csv`: Extended version with indicators  
- `Cleaned Data/`: All cleaned input files  
- `articles_by_date.csv`: News text data by date  
- `R scripts`: Data preprocessing, indicator calculation, and modeling  

---

## 📅 Next Steps

- Implement backtesting on trading strategy using predictions  
- Compare against a buy-and-hold benchmark  
- Explore ensemble and deep learning models for further performance improvement  

---

## 👥 Team

**Group 4 – APAN5205**  
Xikun Feng, Ronghao Zeng, Chen Chen, Yibo Wang, Peiqi Tan

