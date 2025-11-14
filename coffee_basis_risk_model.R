# Financial Risk Management Project – Coffee Basis Risk
# Authors: Federico Frezzotti, Arianna Falasca, Enea Pizzella,
#          Giuseppina Rainone, Lorenzo Barbolla
#
# This script implements regression-based and rolling-window models
# to forecast coffee basis risk using macroeconomic variables and
# Google Trends data.
# To install the necessary libraries 
# install.packages(c("readxl", "dplyr", "caret", "Metrics", "forecast", "tidyverse"))

# To recall the libraries we are going to use
library(readxl)    # To read Excel files
library(caret)     # For creating training/test splits and cross-validation
library(Metrics)   # For calculating RMSE, MAE
library(forecast)  # For time series models (ARIMA, etc.)
library(dplyr)     # for data manipulation
library(tidyverse) # for data manipulation and visualization

# Reading the data from an Excel file
Data_coffee <- read_excel("data/ISTHISTHELASTCOFFEE.xlsx")
# This line reads the data from the specified Excel file and stores it in the 'Data_coffee' data frame.
# When running the code, remember to change the path into the right one for your computer
# just checking the column 11 (irrelevant for the code, just to inspect its values)
Data_coffee$...11
# create a base for the data (to recall the original data if we need them after we change them)
Data_coffee_base <- Data_coffee
# Display the entire DataFrame to see the data structure
Data_coffee
# Displaying column names in the data frame, useful for identifying the structure of the data.
colnames(Data_coffee)

# Assigning dependent variable (basis risk) from the data
dep_var <- Data_coffee$`basis risk...13`
# extracts the 'basis risk' column (renamed to 'basis risk...13' in the dataset)
# and assigns it as the dependent variable for further analysis.

# Now we extract each variable and convert it to numeric (when it is needed)
google_search_trend <- as.numeric(as.character(Data_coffee$`google search trend...14`))
google_news <- as.numeric(as.character(Data_coffee$`google news search trend...15`))
crude_oil <- Data_coffee$`crude oil 1 month future...16`
VIX <- Data_coffee$VIX...17
MOVE <- Data_coffee$MOVE...18
US_FED_middle_rate <- Data_coffee$`US FED effective rate - middle rate...19`
US_NOM_broad_index <- Data_coffee$`us nominal dollar broad index...20`
Open_Interest <- Data_coffee$`Open Interest...21`
# We store now the quarterly dummy variables in separate variables for seasonal effects
Q1 <- Data_coffee$...23
Q2 <- Data_coffee$...24
Q3 <- Data_coffee$...25
Q4 <- Data_coffee$...26
# Just check the value of one of the quarter variables
Data_coffee$...23


# We now fit the initial linear regression model with all variables
model <- lm(dep_var ~ google_search_trend + google_news + crude_oil + VIX + MOVE + US_FED_middle_rate + US_NOM_broad_index + Open_Interest)

# This line displays the summary of the model, including coefficients, R-squared and p-values.
# It helps to assess the performance and significance of each predictor.
summary(model)

# Here we added some additional models that are not important for the final results
# consider only some variables 
 model2 <- lm(dep_var ~ google_search_trend + crude_oil + MOVE + US_FED_middle_rate + US_NOM_broad_index + Open_Interest)

 summary(model2)


 model3 <- lm(dep_var ~ crude_oil + MOVE + US_FED_middle_rate + US_NOM_broad_index)

 summary(model3)


 model4 <- lm(dep_var ~ google_search_trend + crude_oil + MOVE + US_FED_middle_rate + US_NOM_broad_index + Open_Interest)

 summary(model4)

 
# Stepwise selection for model improvement: adds/removes variables to find the optimal model.
stepwise_model <- step(model, direction = "both")
summary(stepwise_model)


# We now fit a seasonal model including quarterly variables to account for seasonality
model_season <- lm(dep_var ~ google_search_trend + google_news + crude_oil + VIX + MOVE + US_FED_middle_rate + US_NOM_broad_index + Open_Interest + Q1 + Q2 + Q3)

summary(model_season)

# Removing rows 91 to 103 from the DataFrame
# Adjusted time window
Data_coffee2 <- Data_coffee[-c(91:103),]

# Rebuild the independent variables for the modified time window
dep_var <- Data_coffee$`basis risk...13`
google_search_trend <- as.numeric(as.character(Data_coffee$`google search trend...14`))
google_news <- as.numeric(as.character(Data_coffee$`google news search trend...15`))
crude_oil <- Data_coffee$`crude oil 1 month future...16`
VIX <- Data_coffee$VIX...17
MOVE <- Data_coffee$MOVE...18
US_FED_middle_rate <- Data_coffee$`US FED effective rate - middle rate...19`
US_NOM_broad_index <- Data_coffee$`us nominal dollar broad index...20`
Open_Interest <- Data_coffee$`Open Interest...21`
Q1 <- Data_coffee$...23
Q2 <- Data_coffee$...24
Q3 <- Data_coffee$...25
Q4 <- Data_coffee$...26

model5 <- lm(dep_var ~ google_search_trend + google_news + crude_oil + VIX + MOVE + US_FED_middle_rate + US_NOM_broad_index + Open_Interest)

summary(model5)






# Recover the original data for the Cross Validation
Data_coffee <- Data_coffee_base
dep_var <- Data_coffee$`basis risk...13`
google_search_trend <- as.numeric(as.character(Data_coffee$`google search trend...14`))
google_news <- as.numeric(as.character(Data_coffee$`google news search trend...15`))
crude_oil <- Data_coffee$`crude oil 1 month future...16`
VIX <- Data_coffee$VIX...17
MOVE <- Data_coffee$MOVE...18
US_FED_middle_rate <- Data_coffee$`US FED effective rate - middle rate...19`
US_NOM_broad_index <- Data_coffee$`us nominal dollar broad index...20`
Open_Interest <- Data_coffee$`Open Interest...21`
# Quarterly seasonal dummies
Q1 <- Data_coffee$...23
Q2 <- Data_coffee$...24
Q3 <- Data_coffee$...25
Q4 <- Data_coffee$...26

# CROSS VALIDATION
# View the structure of the data
head(Data_coffee)
# This function displays the first 6 rows of the 'Data_coffee' dataframe. 
# It allows you to quickly inspect the data structure and understand how the columns are organized.

  
# Convert the 'Date' column to Date format (if not already)
Data_coffee$Date <- as.Date(Data_coffee$Date)
Data_coffee

# Create a time series object with the weekly data 
# We will use 'basis_risk' as the target variable for modeling and adjust frequency to match the data
ts_data <- ts(Data_coffee$`basis risk...13`, frequency = 52, start = c(2022, 23))  
ts_data

# First code: Model prediction evaluation with Rolling Window
# Define the size of the rolling window
# Initial choice for train and test window sizes (adjustable)
train_size <- 60  
test_size <- 10    

# Set up a function for cross-validation using a rolling window
rolling_window <- function(ts_data, independent_vars, train_size, test_size) {
  n <- length(ts_data)  # Number of data points
  results <- data.frame(RMSE = numeric(0), MAE = numeric(0), R2 = numeric(0))
  
  for (start in 1:(n - train_size - test_size + 1)) {
    # Split data into training and testing sets
    train_set <- ts_data[start:(start + train_size - 1)]
    test_set <- ts_data[(start + train_size):(start + train_size + test_size - 1)]
    
    # Subset the independent variables for the current training and test sets
    X_train <- independent_vars[start:(start + train_size - 1), ]
    X_test <- independent_vars[(start + train_size):(start + train_size + test_size - 1), ]
    
    # Fit a Linear Regression model using both dependent and independent variables
    model <- lm(train_set ~ ., data = cbind(train_set, X_train))  
    
    # Predict the next 'test_size' points
    forecast_values <- predict(model, newdata = X_test)
    
    # Compute evaluation metrics
    rmse_val <- sqrt(mean((test_set - forecast_values)^2))
    mae_val <- mean(abs(test_set - forecast_values))
    r2_val <- cor(test_set, forecast_values)^2
    
    # Calculate the results
    results <- rbind(results, data.frame(RMSE = rmse_val, MAE = mae_val, R2 = r2_val))
  }
  
  return(results)
}

# Create a dataframe with all the independent variables
independent_vars <- data.frame(crude_oil=crude_oil,
                               MOVE=MOVE,
                               US_FED_middle_rate=US_FED_middle_rate,
                               US_NOM_broad_index=US_NOM_broad_index)
independent_vars
results <- rolling_window(ts_data, independent_vars, train_size = 65, test_size = 3)
# ts_data is the dependent variable (time series data for basis risk).
# independent_vars contains the independent variables (crude oil, MOVE, interest rates, etc.).
# train_size = 65 sets the number of observations to use in the training set for each rolling window.
# test_size = 3 sets the number of observations in each test set.


# Print the evaluation metrics
print(results)

# Calculate and print the average performances
average_rmse <- mean(results$RMSE)
average_mae <- mean(results$MAE)
average_r2 <- mean(results$R2)

cat("Average RMSE:", average_rmse, "\n")
cat("Average MAE:", average_mae, "\n")
cat("Average R-squared:", average_r2, "\n")







# Second code: Evaluation metrics with time gap in rolling window
# Extracting timestamps from the dataset
timestamps <- Data_coffee$...11
timestamps

# We now define a function for rolling window cross-validation with time gap consideration
rolling_window_with_time_gap <- function(ts_data, independent_vars, timestamps, train_size, test_size) {
  n <- length(ts_data)  # Total number of data points
  results <- data.frame(RMSE = numeric(0), MAE = numeric(0), R2 = numeric(0))  # To store evaluation metrics
  
  # Convert the timestamps to Date type if necessary
  timestamps <- as.Date(timestamps)  
  
  # Calculate the time gap in days between consecutive timestamps
  time_gap <- c(NA, diff(timestamps))  # Difference in days between consecutive timestamps
  
  # Add the time_gap as an additional feature to the independent variables
  independent_vars$time_gap <- time_gap  
  
  # Loop over each rolling window and compute evaluation metrics
  for (start in 1:(n - train_size - test_size + 1)) {  
    # Split data into training and testing sets
    train_set <- ts_data[start:(start + train_size - 1)]  # Training data: first 'train_size' points
    test_set <- ts_data[(start + train_size):(start + train_size + test_size - 1)]  # Test data: next 'test_size' points
    
    # Subset the independent variables for the current training and test sets
    X_train <- independent_vars[start:(start + train_size - 1), ]  # Independent variables for training
    X_test <- independent_vars[(start + train_size):(start + train_size + test_size - 1), ]  # Independent variables for testing
    
    # Fit a Multiple Linear Regression model using both dependent and independent variables
    model <- lm(train_set ~ ., data = cbind(train_set, X_train))  # Train model using all independent vars including time_gap
    
    # Predict the next 'test_size' points using the model
    forecast_values <- predict(model, newdata = cbind(X_test))
    
    # Compute the evaluation metrics for performance
    rmse_val <- sqrt(mean((test_set - forecast_values)^2))  # Root Mean Squared Error
    mae_val <- mean(abs(test_set - forecast_values))  # Mean Absolute Error
    r2_val <- cor(test_set, forecast_values)^2  # R-squared
    
    # Create the results
    results <- rbind(results, data.frame(RMSE = rmse_val, MAE = mae_val, R2 = r2_val))
  }
  
  return(results)  # Return the evaluation metrics for all rolling windows
}

# Compute the evaluation metrics
results <- rolling_window_with_time_gap(ts_data, independent_vars, timestamps, train_size = 60, test_size = 5)
# Print the evaluation metrics
print(results)

# Calculate and print the average performance
average_rmse <- mean(results$RMSE)
average_mae <- mean(results$MAE)
average_r2 <- mean(results$R2)

cat("Average RMSE:", average_rmse, "\n")
cat("Average MAE:", average_mae, "\n")
cat("Average R-squared:", average_r2, "\n")





# Third code: Time-aware Rolling Window
# Function for time-aware rolling window cross-validation using integer time index
time_aware_rolling_window <- function(ts_data, independent_vars, time_index, train_window, test_window) {
  
  # Create an empty dataframe to store results
  results <- data.frame(RMSE = numeric(0), MAE = numeric(0), R2 = numeric(0))
  # Data frame for storing evaluation metrics
  
  # Define start of first training set
  start_index <- 1
  
  while (TRUE) {
    # Define the end of the training window based on time, not the row index
    train_end_time <- time_index[start_index] + train_window  # End of training period
    train_indices <- which(time_index >= time_index[start_index] & time_index <= train_end_time)
    # This line determines the end of the training window by adding 'train_window' (in time steps) to the current start time.
    # train_indices are the data points within this time range: they ensure that the training set is time-based, not just row-based.
    
    # Define test window to be the next test_window steps after training ends
    test_start_time <- max(time_index[train_indices]) + 1
    test_end_time <- test_start_time + test_window - 1
    test_indices <- which(time_index >= test_start_time & time_index <= test_end_time)
    # test_indices contains the indices of the test data points within this time period.
    
    # Stop loop if not enough data left for a test set
    if (length(test_indices) < 2) {
      break
    }
    # This condition ensures that there are enough test data points. If the test set size is less than 2, the loop stops.
    
    # Extract training and testing sets
    train_set <- ts_data[train_indices]
    test_set <- ts_data[test_indices]
    # train_set corresponds to the subset of time series data for the training period
    # test_set is for the testing period.
    
    X_train <- independent_vars[train_indices, ]
    X_test <- independent_vars[test_indices, ]
    # X_train and X_test are the corresponding independent variables for the training and testing sets.
    
    # Train a multiple linear regression model
    model <- train(train_set ~ ., data = cbind(train_set, X_train), method = "lm")
    
    # Predict on test set
    forecast_values <- predict(model, newdata = cbind(X_test))
    
    # Compute the evaluation metrics
    rmse_val <- sqrt(mean((test_set - forecast_values)^2))
    mae_val <- mean(abs(test_set - forecast_values))
    r2_val <- cor(test_set, forecast_values)^2
    
    # Compute the results
    results <- rbind(results, data.frame(RMSE = rmse_val, MAE = mae_val, R2 = r2_val))
    
    # Move to the next rolling window (based on time index)
    next_start_time <- time_index[start_index] + 1  # Move forward by 1 
    start_index <- which(time_index >= next_start_time)[1]
    
    # Stop if we arrived at the end
    if (is.na(start_index) || (length(time_index) - start_index) < test_window) {
      break
    }
  }
  
  return(results)
}

timestamps
# Compute the results
results <- time_aware_rolling_window(ts_data, independent_vars, time_index=timestamps, train_window = 55, test_window = 4)

# Print the results
print(results)

# Compute and print average performance
cat("Average RMSE:", mean(results$RMSE), "\n")
cat("Average MAE:", mean(results$MAE), "\n")
cat("Average R-squared:", mean(results$R2), "\n")


