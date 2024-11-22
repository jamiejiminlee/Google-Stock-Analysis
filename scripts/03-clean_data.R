#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace Setup ####
library(tidyverse)
library(lubridate)
library(zoo)
library(arrow)

#### Load Raw Data ####
raw_google_stock_data <- read_csv("data/01-raw_data/raw_google_data.csv")

#### Data Cleaning ####
price_analysis_data <- raw_google_stock_data %>%
  mutate(
    # Convert date to Date type
    date = as.Date(date),
    
    # Create lagged closing price
    Price_Lag1 = lag(close),
    
    # Calculate daily price difference
    Price_Diff = close - Price_Lag1,
    
    # Calculate daily percentage change in price
    Price_Change_Percent = if_else(
      !is.na(Price_Lag1) & Price_Lag1 != 0,
      (Price_Diff / Price_Lag1) * 100,
      NA_real_
    ),
    
    # Calculate 7-day rolling average of close price
    Weekly_Avg_Close = rollmean(close, k = 7, fill = NA, align = "right"),
    
    # Calculate 30-day rolling average of close price
    Monthly_Avg_Close = rollmean(close, k = 30, fill = NA, align = "right"),
    
    # Calculate 7-day rolling standard deviation (volatility)
    Volatility = rollapply(close, width = 7, FUN = sd, fill = NA, align = "right")
  ) %>%
  # Retain only relevant columns
  select(
    date, symbol, open, high, low, close, volume, adjusted, 
    Price_Lag1, Price_Change_Percent, Weekly_Avg_Close, Monthly_Avg_Close, Volatility
  ) %>%
  # Drop rows with NA values
  drop_na()

#### Save Cleaned Data ####
write_parquet(price_analysis_data, "data/02-analysis_data/google_analysis_data.parquet")

