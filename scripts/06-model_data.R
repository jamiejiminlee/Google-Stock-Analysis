#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Load Libraries ####
library(tidyverse)
library(lubridate)
library(xgboost)
library(Metrics)
library(arrow)

#### Load Cleaned Data ####
price_analysis_data <- read_parquet("data/02-analysis_data/google_analysis_data.parquet")

#### Prepare Dataset ####
# Define features (predictors) and target (response variable)
model_data <- price_analysis_data %>%
  select(
    date, close, Price_Lag1, Price_Change_Percent, 
    Weekly_Avg_Close, Monthly_Avg_Close, Volatility, volume
  ) %>%
  mutate(
    # Convert date to numeric for compatibility with XGBoost
    date_numeric = as.numeric(date)
  ) %>%
  drop_na() # Ensure no NA values

# Split into training (2018–2023) and testing (2024)
train_data <- model_data %>% filter(year(date) < 2024)
test_data <- model_data %>% filter(year(date) == 2024)

# Extract features and target for training
X_train <- train_data %>%
  select(-date, -close) %>%
  as.matrix()
y_train <- train_data$close

# Extract features and target for testing
X_test <- test_data %>%
  select(-date, -close) %>%
  as.matrix()
y_test <- test_data$close

# Convert data to XGBoost DMatrix format
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

#### Hyperparameter Tuning ####
# Define hyperparameters (manual grid search or replace with tuned values)
params <- list(
  objective = "reg:squarederror", # Regression objective
  eta = 0.1,                     # Learning rate
  max_depth = 6,                 # Tree depth
  subsample = 0.8,               # Row subsampling
  colsample_bytree = 0.8,        # Feature subsampling
  gamma = 0,                     # Minimum loss reduction
  min_child_weight = 1           # Minimum sum of weights
)

# Train the model with early stopping
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,                      # Maximum number of boosting rounds
  watchlist = list(train = dtrain, eval = dtest), # Monitor both train and test
  eval_metric = "rmse",               # Metric to optimize
  early_stopping_rounds = 10          # Stop if no improvement in 10 rounds
)

# Save the best number of iterations
best_iteration <- xgb_model$best_iteration
cat("Best Iteration: ", best_iteration, "\n")


#### Save the Model ####
saveRDS(xgb_model, "models/google_model.rds")


