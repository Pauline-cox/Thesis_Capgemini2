# SARIMAX Grid Search with Progress Tracking
# Required packages
library(forecast)
library(dplyr)
library(lubridate)

# Define your target variable
target <- ts(clean_data$total_consumption_kWh)

# Define your exogenous variables matrix (replace with your actual variables)
# Example: xreg_vars <- cbind(clean_data$temperature, clean_data$humidity, ...)
# xreg_vars <- your_exogenous_variables_matrix

# Define training period (Jan 2023 to Sept 2024)
train_start <- as.Date("2023-01-01")
train_end <- as.Date("2024-09-30")

# Check your actual date range
date_range <- range(as.Date(clean_data$interval), na.rm = TRUE)
cat("Your data covers:", date_range[1], "to", date_range[2], "\n")

# Create training indices - using 'interval' column instead of 'datetime'
date_part <- as.Date(clean_data$interval)
train_indices <- which(date_part >= train_start & date_part <= train_end)

# Check if we have valid indices and split data
cat("Training data points found:", length(train_indices), "\n")
if (length(train_indices) == 0) {
  cat("No data in specified date range. Using all data for training.\n")
  y_train <- target
  # xreg_train <- xreg_vars
} else {
  cat("Using filtered training data from", date_range[1], "to", date_range[2], "\n")
  y_train <- target[train_indices]
  # xreg_train <- xreg_vars[train_indices, ]
}

# Grid search around optimal SARIMA(3,0,1)(0,1,0)[168]
param_grid <- expand.grid(
  p = c(2, 3, 4),           # ±1 around optimal p=3
  d = c(0),                 # Keep d=0 (already optimal)
  q = c(0, 1, 2),           # ±1 around optimal q=1
  P = c(0, 1),              # ±1 around optimal P=0 (0 is minimum)
  D = c(1),                 # Keep D=1 (already optimal)
  Q = c(0, 1),              # ±1 around optimal Q=0 (0 is minimum)
  seasonal = c(168)         # Keep seasonal period=168 (weekly for hourly data)
)

# Alternative grids (uncomment one if preferred):

# More conservative search (smaller range)
# param_grid <- expand.grid(
#   p = c(2, 3, 4),           
#   d = c(0),                 
#   q = c(1, 2),              # Only +1 from optimal q=1
#   P = c(0, 1),              
#   D = c(1),                 
#   Q = c(0),                 # Keep Q=0 (optimal)
#   seasonal = c(168)         
# )

# More extensive search (if computational resources allow)
# param_grid <- expand.grid(
#   p = c(1, 2, 3, 4, 5),     # Wider range around p=3
#   d = c(0),                 
#   q = c(0, 1, 2, 3),        # Wider range around q=1
#   P = c(0, 1, 2),           
#   D = c(1),                 
#   Q = c(0, 1, 2),           
#   seasonal = c(168)         
# )

cat("Grid search combinations:", nrow(param_grid), "\n")

# Initialize results storage
results <- data.frame(
  p = integer(), d = integer(), q = integer(),
  P = integer(), D = integer(), Q = integer(),
  AIC = numeric(), AICc = numeric(), BIC = numeric(),
  loglik = numeric(), sigma2 = numeric(),
  convergence = logical(), error_msg = character(),
  stringsAsFactors = FALSE
)

# Function to calculate training set errors
calc_training_errors <- function(fitted_values, actual_values) {
  residuals <- actual_values - fitted_values
  
  # MAPE (Mean Absolute Percentage Error)
  mape <- mean(abs(residuals / actual_values)) * 100
  
  # RMSE (Root Mean Square Error)
  rmse <- sqrt(mean(residuals^2))
  
  # MAE (Mean Absolute Error)
  mae <- mean(abs(residuals))
  
  # R-squared
  ss_res <- sum(residuals^2)
  ss_tot <- sum((actual_values - mean(actual_values))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  return(list(MAPE = mape, RMSE = rmse, MAE = mae, R2 = r_squared))
}

# Start grid search
cat("Starting SARIMAX Grid Search\n")
cat("Total combinations:", nrow(param_grid), "\n")
cat("Estimated time:", nrow(param_grid) * 20, "minutes\n")
cat("===========================================\n\n")

start_time <- Sys.time()

for (i in 1:nrow(param_grid)) {
  cat("Progress:", i, "/", nrow(param_grid), "\n")
  cat("Testing SARIMA(", param_grid$p[i], ",", param_grid$d[i], ",", param_grid$q[i], 
      ")(", param_grid$P[i], ",", param_grid$D[i], ",", param_grid$Q[i], ")[", 
      param_grid$seasonal[i], "]\n")
  
  iter_start <- Sys.time()
  
  tryCatch({
    # Fit SARIMAX model
    model <- Arima(y_train,
                   order = c(param_grid$p[i], param_grid$d[i], param_grid$q[i]),
                   seasonal = list(order = c(param_grid$P[i], param_grid$D[i], param_grid$Q[i]), 
                                   period = param_grid$seasonal[i]),
                   # xreg = xreg_train,  # Uncomment when you have exogenous variables
                   method = "ML")
    
    # Store results
    results[i, ] <- data.frame(
      p = param_grid$p[i], d = param_grid$d[i], q = param_grid$q[i],
      P = param_grid$P[i], D = param_grid$D[i], Q = param_grid$Q[i],
      AIC = model$aic, AICc = model$aicc, BIC = model$bic,
      loglik = model$loglik, sigma2 = model$sigma2,
      convergence = TRUE, error_msg = ""
    )
    
    cat("✓ Success - AIC:", round(model$aic, 2), "BIC:", round(model$bic, 2), "\n")
    
  }, error = function(e) {
    # Store error information
    results[i, ] <- data.frame(
      p = param_grid$p[i], d = param_grid$d[i], q = param_grid$q[i],
      P = param_grid$P[i], D = param_grid$D[i], Q = param_grid$Q[i],
      AIC = NA, AICc = NA, BIC = NA,
      loglik = NA, sigma2 = NA,
      convergence = FALSE, error_msg = as.character(e$message)
    )
    
    cat("✗ Error:", e$message, "\n")
  })
  
  iter_end <- Sys.time()
  iter_time <- as.numeric(difftime(iter_end, iter_start, units = "mins"))
  
  cat("Iteration time:", round(iter_time, 2), "minutes\n")
  
  # Estimate remaining time
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  avg_time_per_iter <- elapsed_time / i
  remaining_time <- (nrow(param_grid) - i) * avg_time_per_iter
  
  cat("Elapsed:", round(elapsed_time, 1), "min | Remaining:", round(remaining_time, 1), "min\n")
  cat("-------------------------------------------\n\n")
}

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")

cat("Grid Search Complete!\n")
cat("Total time:", round(total_time, 2), "minutes\n\n")

# Display results sorted by AIC
results_clean <- results[results$convergence == TRUE, ]
results_sorted <- results_clean[order(results_clean$AIC), ]

cat("=== GRID SEARCH RESULTS (sorted by AIC) ===\n")
print(results_sorted[, c("p", "d", "q", "P", "D", "Q", "AIC", "AICc", "BIC", "loglik")])

# Fit and analyze the best model
if (nrow(results_sorted) > 0) {
  cat("\n=== BEST MODEL ANALYSIS ===\n")
  
  best_params <- results_sorted[1, ]
  cat("Best model: SARIMA(", best_params$p, ",", best_params$d, ",", best_params$q, 
      ")(", best_params$P, ",", best_params$D, ",", best_params$Q, ")[168]\n\n")
  
  # Refit the best model for detailed analysis
  best_model <- Arima(y_train,
                      order = c(best_params$p, best_params$d, best_params$q),
                      seasonal = list(order = c(best_params$P, best_params$D, best_params$Q), 
                                      period = 168),
                      # xreg = xreg_train,  # Uncomment when you have exogenous variables
                      method = "ML")
  
  # Print model summary
  cat("Model Summary:\n")
  print(best_model)
  
  cat("\n=== TRAINING SET ERRORS ===\n")
  
  # Calculate training set errors
  fitted_values <- fitted(best_model)
  training_errors <- calc_training_errors(fitted_values, y_train)
  
  cat("MAPE:", round(training_errors$MAPE, 3), "%\n")
  cat("RMSE:", round(training_errors$RMSE, 4), "\n")
  cat("MAE:", round(training_errors$MAE, 4), "\n")
  cat("R²:", round(training_errors$R2, 4), "\n")
  
  # Residual diagnostics
  cat("\n=== RESIDUAL DIAGNOSTICS ===\n")
  residuals_test <- Box.test(residuals(best_model), lag = 10, type = "Ljung-Box")
  cat("Ljung-Box test p-value:", round(residuals_test$p.value, 4), "\n")
  cat("Residuals independent:", ifelse(residuals_test$p.value > 0.05, "YES", "NO"), "\n")
  
} else {
  cat("No models converged successfully!\n")
}
