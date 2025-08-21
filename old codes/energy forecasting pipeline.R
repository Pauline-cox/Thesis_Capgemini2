# q.forecasting_pipeline.R
# -----------------------------------------------------------------------------
# Energy consumption forecasting with SARIMA, SARIMAX, and LSTM
# Multiple horizons: 24h, 1-week, 4-weeks with walk-forward validation
# -----------------------------------------------------------------------------

# Load required libraries
library(forecast)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(keras)
library(tensorflow)
library(caret)

# =============================================================================
# 1. CONFIGURATION AND HELPER FUNCTIONS
# =============================================================================

# Forecasting configuration
HORIZONS <- list(
  h24 = 24,      # 24 hours
  h168 = 168,    # 1 week  
  h672 = 672     # 4 weeks
)

# Evaluation metrics
calculate_metrics <- function(actual, predicted) {
  # Remove NA values
  valid_idx <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid_idx]
  predicted <- predicted[valid_idx]
  
  if(length(actual) == 0) return(list(MAE = NA, RMSE = NA, MAPE = NA, sMAPE = NA))
  
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  smape <- mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) * 100
  
  return(list(MAE = mae, RMSE = rmse, MAPE = mape, sMAPE = smape))
}

# Time series cross-validation split
create_cv_splits <- function(data, initial_train_size, horizon, n_splits = 10) {
  n_obs <- nrow(data)
  max_train_end <- n_obs - horizon - n_splits + 1
  
  if(initial_train_size > max_train_end) {
    stop("Not enough data for the specified configuration")
  }
  
  splits <- list()
  for(i in 1:n_splits) {
    train_end <- initial_train_size + (i - 1)
    test_start <- train_end + 1
    test_end <- test_start + horizon - 1
    
    if(test_end > n_obs) break
    
    splits[[i]] <- list(
      train_idx = 1:train_end,
      test_idx = test_start:test_end,
      split_id = i
    )
  }
  return(splits)
}

# =============================================================================
# 2. SARIMA MODEL FUNCTIONS
# =============================================================================

fit_sarima <- function(train_data) {
  # Convert to ts object with hourly frequency
  ts_data <- ts(train_data$total_consumption_kWh, frequency = 24)
  
  # Auto ARIMA with seasonal components
  model <- tryCatch({
    auto.arima(ts_data, 
               seasonal = TRUE,
               stepwise = FALSE,
               approximation = FALSE,
               max.order = 10,
               max.P = 2, max.Q = 2, max.D = 1)
  }, error = function(e) {
    # Fallback to simpler model
    auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE)
  })
  
  return(model)
}

forecast_sarima <- function(model, horizon) {
  forecast_result <- forecast(model, h = horizon)
  return(as.numeric(forecast_result$mean))
}

# =============================================================================
# 3. SARIMAX MODEL FUNCTIONS  
# =============================================================================

fit_sarimax <- function(train_data) {
  # Prepare exogenous variables (exclude target and time columns)
  exog_vars <- c("total_occupancy", "tempC", "humidity", "co2", "sound", "lux",
                 "temperature", "wind_speed", "sunshine_minutes", "global_radiation",
                 "humidity_percent", "fog", "rain", "snow", "thunder", "ice",
                 "hour", "office_hours", "is_weekend", "hour_sin", "hour_cos", "holiday")
  
  # Select available exogenous variables
  available_vars <- intersect(exog_vars, names(train_data))
  xreg_train <- as.matrix(train_data[, ..available_vars])
  
  # Convert target to ts
  ts_data <- ts(train_data$total_consumption_kWh, frequency = 24)
  
  # Fit ARIMA with external regressors
  model <- tryCatch({
    auto.arima(ts_data, 
               xreg = xreg_train,
               seasonal = TRUE,
               stepwise = FALSE,
               approximation = FALSE,
               max.order = 8)
  }, error = function(e) {
    auto.arima(ts_data, xreg = xreg_train, seasonal = TRUE, stepwise = TRUE)
  })
  
  return(list(model = model, exog_vars = available_vars))
}

forecast_sarimax <- function(model_obj, test_data, horizon) {
  model <- model_obj$model
  exog_vars <- model_obj$exog_vars
  
  # Prepare future exogenous variables
  if(nrow(test_data) >= horizon) {
    xreg_future <- as.matrix(test_data[1:horizon, ..exog_vars])
  } else {
    # If not enough future data, use last available values
    last_vals <- test_data[nrow(test_data), ..exog_vars]
    xreg_future <- matrix(rep(as.numeric(last_vals), horizon), 
                          nrow = horizon, byrow = TRUE)
    colnames(xreg_future) <- exog_vars
  }
  
  forecast_result <- forecast(model, h = horizon, xreg = xreg_future)
  return(as.numeric(forecast_result$mean))
}

# =============================================================================
# 4. LSTM MODEL FUNCTIONS
# =============================================================================

# Prepare LSTM data
prepare_lstm_data <- function(data, lookback = 168, horizon = 24) {
  # Select features for LSTM
  feature_cols <- c("total_consumption_kWh", "total_occupancy", "tempC", "humidity", 
                    "co2", "sound", "lux", "temperature", "wind_speed", 
                    "office_hours", "is_weekend", "hour_sin", "hour_cos")
  
  available_features <- intersect(feature_cols, names(data))
  lstm_data <- as.matrix(data[, ..available_features])
  
  # Create sequences
  n_samples <- nrow(lstm_data) - lookback - horizon + 1
  if(n_samples <= 0) return(NULL)
  
  X <- array(0, dim = c(n_samples, lookback, ncol(lstm_data)))
  y <- array(0, dim = c(n_samples, horizon))
  
  for(i in 1:n_samples) {
    X[i, , ] <- lstm_data[i:(i + lookback - 1), ]
    y[i, ] <- lstm_data[(i + lookback):(i + lookback + horizon - 1), 1]
  }
  
  return(list(X = X, y = y, features = available_features))
}

fit_lstm <- function(train_data, horizon = 24, lookback = 168) {
  lstm_prep <- prepare_lstm_data(train_data, lookback = lookback, horizon = horizon)
  
  if(is.null(lstm_prep) || dim(lstm_prep$X)[1] < 10) {
    return(NULL)  # Not enough data
  }
  
  X_train <- lstm_prep$X
  y_train <- lstm_prep$y
  
  # Build LSTM model
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, return_sequences = TRUE, 
               input_shape = c(lookback, dim(X_train)[3])) %>%
    layer_dropout(rate = 0.2) %>%
    layer_lstm(units = 50, return_sequences = FALSE) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = horizon)
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = 'mse',
    metrics = c('mae')
  )
  
  # Train model
  history <- model %>% fit(
    X_train, y_train,
    epochs = 50,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 0,
    callbacks = list(
      callback_early_stopping(patience = 10, restore_best_weights = TRUE)
    )
  )
  
  return(list(model = model, features = lstm_prep$features, lookback = lookback))
}

forecast_lstm <- function(model_obj, data, horizon) {
  if(is.null(model_obj)) return(rep(NA, horizon))
  
  model <- model_obj$model
  features <- model_obj$features
  lookback <- model_obj$lookback
  
  # Prepare input data
  input_data <- as.matrix(data[, ..features])
  
  if(nrow(input_data) < lookback) {
    return(rep(NA, horizon))  # Not enough data
  }
  
  # Take last 'lookback' observations
  X_pred <- array(input_data[(nrow(input_data) - lookback + 1):nrow(input_data), ],
                  dim = c(1, lookback, length(features)))
  
  # Predict
  predictions <- model %>% predict(X_pred, verbose = 0)
  return(as.numeric(predictions[1, ]))
}

# =============================================================================
# 5. MAIN FORECASTING PIPELINE
# =============================================================================

run_forecasting_experiment <- function(model_data, horizons = HORIZONS) {
  # Ensure data is sorted by time
  model_data <- model_data[order(interval)]
  
  # Initialize results storage
  results <- list()
  
  # Calculate initial training size (18 months)
  total_hours <- nrow(model_data)
  initial_train_size <- min(18 * 30 * 24, floor(total_hours * 0.75))  # 18 months or 75% of data
  
  cat("Total observations:", total_hours, "\n")
  cat("Initial training size:", initial_train_size, "\n")
  
  # Loop through each horizon
  for(horizon_name in names(horizons)) {
    horizon <- horizons[[horizon_name]]
    cat("\n=== Processing horizon:", horizon_name, "(", horizon, "hours) ===\n")
    
    # Create cross-validation splits
    cv_splits <- create_cv_splits(model_data, initial_train_size, horizon, n_splits = 5)
    
    if(length(cv_splits) == 0) {
      cat("Not enough data for horizon", horizon_name, "\n")
      next
    }
    
    # Initialize results for this horizon
    horizon_results <- list()
    
    # Loop through CV splits
    for(split_idx in 1:length(cv_splits)) {
      split <- cv_splits[[split_idx]]
      train_data <- model_data[split$train_idx]
      test_data <- model_data[split$test_idx]
      actual_values <- test_data$total_consumption_kWh
      
      cat("Split", split_idx, "- Training size:", length(split$train_idx), 
          "Test size:", length(split$test_idx), "\n")
      
      # === SARIMA ===
      cat("  Fitting SARIMA...")
      sarima_start_time <- Sys.time()
      tryCatch({
        sarima_model <- fit_sarima(train_data)
        sarima_pred <- forecast_sarima(sarima_model, horizon)
        sarima_metrics <- calculate_metrics(actual_values, sarima_pred)
        sarima_time <- as.numeric(Sys.time() - sarima_start_time)
        cat(" Done (", round(sarima_time, 2), "s)\n")
      }, error = function(e) {
        cat(" Failed:", e$message, "\n")
        sarima_metrics <- list(MAE = NA, RMSE = NA, MAPE = NA, sMAPE = NA)
        sarima_time <- NA
      })
      
      # === SARIMAX ===
      cat("  Fitting SARIMAX...")
      sarimax_start_time <- Sys.time()
      tryCatch({
        sarimax_model <- fit_sarimax(train_data)
        sarimax_pred <- forecast_sarimax(sarimax_model, 
                                         model_data[(max(split$train_idx) + 1):nrow(model_data)], 
                                         horizon)
        sarimax_metrics <- calculate_metrics(actual_values, sarimax_pred)
        sarimax_time <- as.numeric(Sys.time() - sarimax_start_time)
        cat(" Done (", round(sarimax_time, 2), "s)\n")
      }, error = function(e) {
        cat(" Failed:", e$message, "\n")
        sarimax_metrics <- list(MAE = NA, RMSE = NA, MAPE = NA, sMAPE = NA)
        sarimax_time <- NA
      })
      
      # === LSTM ===
      cat("  Fitting LSTM...")
      lstm_start_time <- Sys.time()
      tryCatch({
        lstm_model <- fit_lstm(train_data, horizon = horizon)
        lstm_pred <- forecast_lstm(lstm_model, train_data, horizon)
        lstm_metrics <- calculate_metrics(actual_values, lstm_pred)
        lstm_time <- as.numeric(Sys.time() - lstm_start_time)
        cat(" Done (", round(lstm_time, 2), "s)\n")
      }, error = function(e) {
        cat(" Failed:", e$message, "\n")
        lstm_metrics <- list(MAE = NA, RMSE = NA, MAPE = NA, sMAPE = NA)
        lstm_time <- NA
      })
      
      # Store results for this split
      horizon_results[[paste0("split_", split_idx)]] <- list(
        split_id = split_idx,
        train_start = model_data$interval[split$train_idx[1]],
        train_end = model_data$interval[split$train_idx[length(split$train_idx)]],
        test_start = model_data$interval[split$test_idx[1]],
        test_end = model_data$interval[split$test_idx[length(split$test_idx)]],
        sarima = c(sarima_metrics, training_time = sarima_time),
        sarimax = c(sarimax_metrics, training_time = sarimax_time),
        lstm = c(lstm_metrics, training_time = lstm_time)
      )
    }
    
    results[[horizon_name]] <- horizon_results
  }
  
  return(results)
}

# =============================================================================
# 6. RESULTS ANALYSIS AND VISUALIZATION
# =============================================================================

analyze_results <- function(results) {
  # Create summary data frame
  summary_df <- data.frame()
  
  for(horizon_name in names(results)) {
    horizon_results <- results[[horizon_name]]
    
    for(model_name in c("sarima", "sarimax", "lstm")) {
      # Extract metrics across all splits
      mae_vals <- sapply(horizon_results, function(x) x[[model_name]]$MAE)
      rmse_vals <- sapply(horizon_results, function(x) x[[model_name]]$RMSE)
      mape_vals <- sapply(horizon_results, function(x) x[[model_name]]$MAPE)
      smape_vals <- sapply(horizon_results, function(x) x[[model_name]]$sMAPE)
      time_vals <- sapply(horizon_results, function(x) x[[model_name]]$training_time)
      
      # Calculate summary statistics
      summary_df <- rbind(summary_df, data.frame(
        Horizon = horizon_name,
        Model = toupper(model_name),
        MAE_mean = mean(mae_vals, na.rm = TRUE),
        MAE_sd = sd(mae_vals, na.rm = TRUE),
        RMSE_mean = mean(rmse_vals, na.rm = TRUE),
        RMSE_sd = sd(rmse_vals, na.rm = TRUE),
        MAPE_mean = mean(mape_vals, na.rm = TRUE),
        MAPE_sd = sd(mape_vals, na.rm = TRUE),
        sMAPE_mean = mean(smape_vals, na.rm = TRUE),
        sMAPE_sd = sd(smape_vals, na.rm = TRUE),
        Time_mean = mean(time_vals, na.rm = TRUE),
        Time_sd = sd(time_vals, na.rm = TRUE)
      ))
    }
  }
  
  return(summary_df)
}

plot_results <- function(summary_df) {
  # Reshape for plotting
  library(tidyr)
  library(ggplot2)
  
  plot_data <- summary_df %>%
    select(Horizon, Model, MAE_mean, RMSE_mean, MAPE_mean, sMAPE_mean) %>%
    pivot_longer(cols = c(MAE_mean, RMSE_mean, MAPE_mean, sMAPE_mean),
                 names_to = "Metric", values_to = "Value") %>%
    mutate(Metric = gsub("_mean", "", Metric))
  
  # Create comparison plots
  p1 <- ggplot(plot_data, aes(x = Horizon, y = Value, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Metric, scales = "free_y") +
    theme_minimal() +
    labs(title = "Model Performance Comparison Across Horizons",
         x = "Forecast Horizon", y = "Metric Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p1)
}

# =============================================================================
# 7. MAIN EXECUTION FUNCTION
# =============================================================================

main_forecasting_pipeline <- function(model_data) {
  cat("Starting Energy Consumption Forecasting Pipeline\n")
  cat("================================================\n")
  
  # Check if Keras/TensorFlow is properly configured
  if(!py_module_available("tensorflow")) {
    cat("Warning: TensorFlow not available. LSTM models will be skipped.\n")
  }
  
  # Run experiments
  results <- run_forecasting_experiment(model_data)
  
  # Analyze results
  cat("\n=== ANALYSIS ===\n")
  summary_results <- analyze_results(results)
  
  # Print summary table
  print(summary_results)
  
  # Create visualizations
  plot_obj <- plot_results(summary_results)
  print(plot_obj)
  
  # Return all results
  return(list(
    detailed_results = results,
    summary = summary_results,
    plot = plot_obj
  ))
}