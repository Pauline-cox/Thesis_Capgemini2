# Load required libraries
suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
  library(ggplot2)
  library(lubridate)
  library(tseries)
  library(moments)
  library(keras)
})

# Configuration
TRAIN_START <- as.POSIXct("2023-09-01 00:00:00", tz = "UTC")
TRAIN_END <- as.POSIXct("2024-09-30 23:00:00", tz = "UTC")
TEST_START <- as.POSIXct("2024-10-01 00:00:00", tz = "UTC")
TEST_END <- as.POSIXct("2024-12-14 23:00:00", tz = "UTC")
ROLLING_STEPS <- 24
LSTM_HORIZON <- 24

# Prepare data
prepare_data <- function(dt) {
  setorder(dt, interval)
  train_dt <- dt[interval >= TRAIN_START & interval <= TRAIN_END]
  test_dt <- dt[interval >= TEST_START & interval <= TEST_END]
  list(train = train_dt, test = test_dt)
}

# SARIMA model
run_sarima <- function(train_ts, test_vals, test_dt) {
  h <- length(test_vals)
  all_forecasts <- numeric(0)
  all_actuals <- numeric(0)
  all_timestamps <- as.POSIXct(character(0))
  
  for (i in seq(1, h, by = ROLLING_STEPS)) {
    current_end <- min(i + ROLLING_STEPS - 1, h)
    current_test <- test_vals[i:current_end]
    current_train <- ts(c(train_ts, test_vals[0:(i-1)]), frequency = 168)
    
    fit <- Arima(current_train,
                 order = c(1,0,1),
                 seasonal = list(order = c(1,1,1), period = 168),
                 include.mean = TRUE,
                 method = "CSS")
    
    fc <- forecast(fit, h = ROLLING_STEPS)
    current_forecast <- as.numeric(fc$mean)[1:length(current_test)]
    
    all_forecasts <- c(all_forecasts, current_forecast)
    all_actuals <- c(all_actuals, current_test)
    all_timestamps <- c(all_timestamps, test_dt$interval[i:current_end])
  }
  
  data.table(interval = all_timestamps,
             actual = all_actuals,
             forecast = all_forecasts,
             model = "SARIMA")
}

# SARIMAX model
run_sarimax <- function(train_dt, test_dt, xreg_vars) {
  y_train <- ts(train_dt$total_consumption_kWh, frequency = 168)
  y_test <- test_dt$total_consumption_kWh
  X_train <- as.matrix(train_dt[, ..xreg_vars])
  X_test <- as.matrix(test_dt[, ..xreg_vars])
  h <- length(y_test)
  
  all_forecasts <- numeric(0)
  all_actuals <- numeric(0)
  all_timestamps <- as.POSIXct(character(0))
  
  for (i in seq(1, h, by = ROLLING_STEPS)) {
    current_end <- min(i + ROLLING_STEPS - 1, h)
    current_test <- y_test[i:current_end]
    current_train <- ts(c(y_train, y_test[0:(i-1)]), frequency = 168)
    X_train_ext <- rbind(X_train, X_test[0:(i-1), , drop = FALSE])
    X_test_ext <- X_test[i:current_end, , drop = FALSE]
    
    fit <- Arima(current_train,
                 order = c(1,0,1),
                 seasonal = list(order = c(1,1,1), period = 168),
                 xreg = X_train_ext,
                 include.mean = TRUE,
                 method = "CSS")
    
    fc <- forecast(fit, h = length(current_test), xreg = X_test_ext)
    current_forecast <- as.numeric(fc$mean)
    
    all_forecasts <- c(all_forecasts, current_forecast)
    all_actuals <- c(all_actuals, current_test)
    all_timestamps <- c(all_timestamps, test_dt$interval[i:current_end])
  }
  
  data.table(interval = all_timestamps,
             actual = all_actuals,
             forecast = all_forecasts,
             model = "SARIMAX")
}

run_lstm <- function(train_dt, test_dt) {
  
  # feature_columns <- c(
  #   "total_occupancy","co2","tempC","humidity","sound","lux",
  #   "temperature","wind_speed","sunshine_minutes","global_radiation","humidity_percent",
  #   "holiday","office_hours","is_weekend",
  #   "hour_sin","hour_cos","weekday","month","year",
  #   # lags
  #   "lag_24","lag_48","lag_72","lag_168","lag_336","lag_504","lag_720",
  #   # rolls
  #   "rollmean_24","rollmean_168","rollmean_720",
  #   "rollsd_24","rollsd_168","rollsd_720"
  # )
  feature_columns <- c(
    "total_occupancy", "co2", "tempC", "humidity", "sound", "lux",
    "temperature", "global_radiation",
    "holiday", "office_hours", "is_weekend",
    "hour_sin", "hour_cos", "weekday",
    "lag_24", "lag_72", "lag_168", "lag_336", "lag_504",
    "rollmean_24", "rollmean_168"
  )

  feature_columns <- intersect(feature_columns, names(train_dt))
  
  lookback <- 96
  horizon  <- 24
  
  # ==== 2. Preprocessing with recipe ====
  train_dt[, target := total_consumption_kWh]
  test_dt[,  target := total_consumption_kWh]
  all_data <- rbind(train_dt, test_dt)
  
  rec <- recipe(target ~ ., data = all_data[, c("target", feature_columns), with = FALSE]) %>%
    # Treat weekday as nominal factor (will one-hot correctly)
    step_mutate(weekday = as.factor(weekday)) %>%
    # One-hot encode
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    # Remove zero-variance predictors (avoids NaN from constant cols)
    step_zv(all_predictors()) %>%
    # Scale numeric features to [0,1]
    step_range(all_numeric(), -all_outcomes(), min = 0, max = 1) %>%
    prep(training = train_dt)
  
  norm_train <- bake(rec, train_dt)
  norm_test  <- bake(rec, test_dt)
  
  predictors <- setdiff(names(norm_train), "target")
  
  X_train <- as.matrix(norm_train[, predictors, drop = FALSE])
  X_test  <- as.matrix(norm_test[, predictors, drop = FALSE])
  
  y_train <- train_dt$target
  y_test  <- test_dt$target
  
  # ==== 3. Scale target based on train only ====
  y_min <- min(y_train, na.rm = TRUE)
  y_max <- max(y_train, na.rm = TRUE)
  scale_y  <- function(y) (y - y_min) / (y_max - y_min + 1e-6)
  invert_y <- function(y) y * (y_max - y_min) + y_min
  
  y_train_scaled <- scale_y(y_train)
  y_test_scaled  <- scale_y(y_test)
  
  # ==== 4. Create LSTM datasets ====
  create_lstm_dataset <- function(X, y, lookback, horizon) {
    n_samples <- nrow(X) - lookback - horizon + 1
    X_array <- array(NA_real_, dim = c(n_samples, lookback, ncol(X)))
    y_array <- array(NA_real_, dim = c(n_samples, horizon))
    for (i in 1:n_samples) {
      X_array[i,,] <- X[i:(i + lookback - 1), ]
      y_array[i,]  <- y[(i + lookback):(i + lookback + horizon - 1)]
    }
    list(X = X_array, y = y_array)
  }
  
  train_data <- create_lstm_dataset(X_train, y_train_scaled, lookback, horizon)
  test_data  <- create_lstm_dataset(X_test,  y_test_scaled,  lookback, horizon)
  
  # ==== 5. Model ====
  model <- keras_model_sequential() %>%
    layer_lstm(units = 128, input_shape = c(lookback, ncol(X_train)),
               return_sequences = TRUE) %>%
    layer_dropout(rate = 0.2) %>%
    layer_lstm(units = 64, return_sequences = FALSE) %>%
    layer_dense(units = horizon, activation = "linear")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = "mse",
    metrics = "mae"
  )
  
  history <- model %>% fit(
    train_data$X, train_data$y,
    epochs = 150, batch_size = 32,
    validation_split = 0.2,
    verbose = 1,
    callbacks = list(
      callback_early_stopping(patience = 10, restore_best_weights = TRUE)
    )
  )
  
  # ==== 6. Forecasting ====
  n_test <- nrow(X_test)
  forecasts <- numeric(0)
  actuals   <- numeric(0)
  stamps    <- as.POSIXct(character(0))
  
  for (i in seq(1, n_test - lookback - horizon + 1, by = horizon)) {
    X_input <- array_reshape(X_test[i:(i + lookback - 1), ],
                             c(1, lookback, ncol(X_test)))
    pred_scaled <- predict(model, X_input, verbose = 0)
    preds <- invert_y(as.numeric(pred_scaled))
    idx <- (i + lookback):(i + lookback + horizon - 1)
    actual <- y_test[idx]
    
    forecasts <- c(forecasts, preds)
    actuals   <- c(actuals, actual)
    stamps    <- c(stamps, test_dt$interval[idx])
  }
  
  data.table(interval = stamps,
             actual = actuals,
             forecast = forecasts,
             model = "LSTM")
}

run_lstm_tuned <- function(train_dt, test_dt) {
  # --- Hyperparams from BO (mapped to what your code actually uses)
  lookback <- 96        # BO gave 105, mapped to 96 in your objective
  units1   <- 70L
  units2   <- 63L
  dropout  <- 0.231
  lr       <- 0.001950638
  batch_sz <- 32L       # BO gave 41, mapped to 32
  horizon  <- 24
  
  feature_columns <- c(
    "total_occupancy","co2","tempC","humidity","sound","lux",
    "temperature","global_radiation",
    "holiday","office_hours","is_weekend",
    "hour_sin","hour_cos","weekday",
    "lag_24","lag_72","lag_168","lag_336","lag_504",
    "rollmean_24","rollmean_168"
  )
  feature_columns <- intersect(feature_columns, names(train_dt))
  
  # --- Preprocess (no leakage)
  train_dt[, target := total_consumption_kWh]
  test_dt[,  target := total_consumption_kWh]
  all_data <- rbind(train_dt, test_dt)
  
  rec <- recipes::recipe(target ~ ., data = all_data[, c("target", feature_columns), with = FALSE]) %>%
    # safer than step_mutate; converts characters → factors (skips factors)
    step_string2factor(all_nominal(), -all_outcomes()) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    step_range(all_numeric(), -all_outcomes()) %>%
    prep(training = train_dt)
  
  norm_train <- bake(rec, train_dt)
  norm_test  <- bake(rec, test_dt)
  predictors <- setdiff(names(norm_train), "target")
  
  X_train <- as.matrix(norm_train[, predictors, drop = FALSE])
  X_test  <- as.matrix(norm_test[, predictors, drop = FALSE])
  
  y_train <- train_dt$target
  y_test  <- test_dt$target
  
  # --- Target scaling on train only
  y_min <- min(y_train, na.rm = TRUE)
  y_max <- max(y_train, na.rm = TRUE)
  scale_y  <- function(y) (y - y_min) / (y_max - y_min + 1e-6)
  invert_y <- function(y) y * (y_max - y_min) + y_min
  
  y_train_scaled <- scale_y(y_train)
  y_test_scaled  <- scale_y(y_test)
  
  # --- Sequences (multi-output)
  create_lstm_dataset <- function(X, y, lookback, horizon) {
    n_samples <- nrow(X) - lookback - horizon + 1
    if (n_samples <= 0) stop("Not enough rows for chosen lookback/horizon.")
    X_array <- array(NA_real_, dim = c(n_samples, lookback, ncol(X)))
    y_array <- array(NA_real_, dim = c(n_samples, horizon))
    for (i in 1:n_samples) {
      X_array[i,,] <- X[i:(i + lookback - 1), ]
      y_array[i,]  <- y[(i + lookback):(i + lookback + horizon - 1)]
    }
    list(X = X_array, y = y_array)
  }
  
  train_data <- create_lstm_dataset(X_train, y_train_scaled, lookback, horizon)
  
  # --- Model (BO picks)
  model <- keras_model_sequential() %>%
    layer_lstm(units = units1, input_shape = c(lookback, ncol(X_train)), return_sequences = TRUE) %>%
    layer_dropout(rate = dropout) %>%
    layer_lstm(units = units2, return_sequences = FALSE) %>%
    layer_dense(units = horizon, activation = "linear")
  
  model %>% compile(
    optimizer = optimizer_nadam(learning_rate = lr),
    loss = "mse",
    metrics = "mae"
  )
  
  history <- model %>% fit(
    x = train_data$X, y = train_data$y,
    epochs = 120,                 # a bit longer than BO’s 60, still early-stopped
    batch_size = batch_sz,
    validation_split = 0.2,
    verbose = 1,
    callbacks = list(
      callback_early_stopping(patience = 10, restore_best_weights = TRUE),
      callback_reduce_lr_on_plateau(factor = 0.5, patience = 5)
    )
  )
  
  # --- Forecast over test in rolling 24h steps
  n_test <- nrow(X_test)
  forecasts <- numeric(0); actuals <- numeric(0); stamps <- as.POSIXct(character(0))
  
  for (i in seq(1, n_test - lookback - horizon + 1, by = horizon)) {
    X_input <- array(X_test[i:(i + lookback - 1), ], dim = c(1, lookback, ncol(X_test)))
    pred_scaled <- predict(model, X_input, verbose = 0)
    preds <- invert_y(as.numeric(pred_scaled))
    idx <- (i + lookback):(i + lookback + horizon - 1)
    actual <- y_test[idx]
    
    forecasts <- c(forecasts, preds)
    actuals   <- c(actuals, actual)
    stamps    <- c(stamps, test_dt$interval[idx])
  }
  
  data.table(interval = stamps,
             actual = actuals,
             forecast = forecasts,
             model = "LSTM_tuned_BO")
}

# ----------------------------
# HYBRID: SARIMAX + LSTM (residual)
# ----------------------------
run_hybrid <- function(train_dt, test_dt, xreg_vars) {
  # 1) Base ARIMAX on TRAIN (in-sample residuals)
  y_train <- ts(train_dt$total_consumption_kWh, frequency = 168)
  X_train <- as.matrix(train_dt[, ..xreg_vars])
  base_fit <- Arima(
    y = y_train,
    order = c(1,0,1),
    seasonal = list(order = c(1,1,1), period = 168),
    xreg = X_train,
    include.mean = TRUE,
    method = "CSS"
  )
  resid_train <- as.numeric(residuals(base_fit))
  
  # 2) Residual LSTM features (same style as your LSTM)
  feature_columns <- c(
    "total_occupancy", "co2", "tempC", "humidity", "sound", "lux",
    "temperature", "global_radiation",
    "holiday", "office_hours", "is_weekend",
    "hour_sin", "hour_cos", "weekday",
    "lag_24", "lag_72", "lag_168", "lag_336", "lag_504",
    "rollmean_24", "rollmean_168"
  )
  feature_columns <- intersect(feature_columns, names(train_dt))
  
  # Build data frames with residual target on TRAIN
  train_res <- copy(train_dt); test_res <- copy(test_dt)
  train_res[, resid_target := resid_train]
  test_res[,  resid_target := NA_real_]
  
  all_res <- rbind(train_res, test_res)
  
  rec <- recipe(resid_target ~ ., data = all_res[, c("resid_target", feature_columns), with = FALSE]) %>%
    step_string2factor(all_nominal(), -all_outcomes()) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    step_range(all_numeric(), -all_outcomes()) %>%
    prep(training = train_res)
  
  norm_tr <- bake(rec, train_res)
  norm_te <- bake(rec, test_res)
  
  predictors <- setdiff(names(norm_tr), "resid_target")
  X_tr <- as.matrix(norm_tr[, predictors, drop = FALSE])
  X_te <- as.matrix(norm_te[, predictors, drop = FALSE])
  
  # Scale residual target on TRAIN only
  rmin <- min(resid_train, na.rm = TRUE)
  rmax <- max(resid_train, na.rm = TRUE)
  scale_r  <- function(y) (y - rmin) / (rmax - rmin + 1e-6)
  invert_r <- function(y) y * (rmax - rmin) + rmin
  resid_train_s <- scale_r(resid_train)
  
  # Sequence maker
  make_seq <- function(X, y, lookback, horizon) {
    n <- nrow(X) - lookback - horizon + 1
    stopifnot(n > 0)
    Xarr <- array(NA_real_, dim = c(n, lookback, ncol(X)))
    Yarr <- array(NA_real_, dim = c(n, horizon))
    for (i in 1:n) {
      Xarr[i,,] <- X[i:(i+lookback-1), ]
      Yarr[i, ] <- y[(i+lookback):(i+lookback+horizon-1)]
    }
    list(X = Xarr, y = Yarr)
  }
  
  lookback <- 96L; horizon <- 24L
  seq_tr <- make_seq(X_tr, resid_train_s, lookback, horizon)
  
  # Residual LSTM
  model_r <- keras_model_sequential() %>%
    layer_lstm(units = 64, input_shape = c(lookback, ncol(X_tr)), return_sequences = TRUE) %>%
    layer_dropout(rate = 0.2) %>%
    layer_lstm(units = 32, return_sequences = FALSE) %>%
    layer_dense(units = horizon, activation = "linear")
  
  model_r %>% compile(optimizer = optimizer_adam(learning_rate = 0.001), loss = "mse")
  
  model_r %>% fit(seq_tr$X, seq_tr$y, epochs = 80, batch_size = 32,
                  validation_split = 0.2, verbose = 1,
                  callbacks = list(callback_early_stopping(patience = 8, restore_best_weights = TRUE)))
  
  # 3) Base SARIMAX rolling forecasts across TEST (for combination)
  base_dt <- run_sarimax(train_dt, test_dt, xreg_vars)
  
  # 4) Residual LSTM rolling forecasts across TEST (same windowing as LSTM)
  n_test <- nrow(X_te)
  r_fc <- numeric(0); ts_out <- as.POSIXct(character(0))
  
  for (i in seq(1, n_test - lookback - horizon + 1, by = horizon)) {
    X_input <- array(X_te[i:(i + lookback - 1), ], dim = c(1, lookback, ncol(X_te)))
    pred_s  <- predict(model_r, X_input, verbose = 0)
    preds   <- invert_r(as.numeric(pred_s))
    idx <- (i + lookback):(i + lookback + horizon - 1)
    
    r_fc   <- c(r_fc, preds)
    ts_out <- c(ts_out, test_dt$interval[idx])
  }
  
  # 5) Hybrid = base + residual (align on overlapping timestamps)
  base_sub <- base_dt[, .(interval, actual, base = forecast)]
  res_dt   <- data.table(interval = ts_out, rhat = r_fc)
  hybrid   <- merge(base_sub, res_dt, by = "interval", all = FALSE)
  hybrid[, forecast := base + rhat]
  
  data.table(interval = hybrid$interval,
             actual   = hybrid$actual,
             forecast = hybrid$forecast,
             model    = "HYBRID_SARIMAX+LSTM")
}


# Main execution
data_list <- prepare_data(model_data)

# Run models
sarima_results <- run_sarima(
  ts(data_list$train$total_consumption_kWh, frequency = 168),
  data_list$test$total_consumption_kWh,
  data_list$test
)

sarimax_results <- run_sarimax(
  data_list$train,
  data_list$test,
  c("total_occupancy", "co2", "lux", "lag_24", "global_radiation", "holiday", "lag_168")
  )

lstm_results <- run_lstm(data_list$train, data_list$test)
lstm_results_tuned <- run_lstm_tuned(data_list$train, data_list$test)

hybrid_results <- run_hybrid(
  data_list$train,
  data_list$test,
  c("total_occupancy", "co2", "lux", "lag_24", "global_radiation", "holiday", "lag_168")
)

# Combine results
all_results <- rbindlist(list(
  sarima_results, sarimax_results, lstm_results, lstm_results_tuned, hybrid_results
), use.names = TRUE)
# Combine results
all_results <- rbindlist(list(
  sarima_results, lstm_results_tuned, hybrid_results
), use.names = TRUE)


all_results[, residual := actual - forecast]

# Calculate metrics
eps <- 1e-6
metrics <- all_results[, .(
  RMSE  = sqrt(mean(residual^2)),
  MAE   = mean(abs(residual)),
  # Robust MAPE: ignore points where actual ~ 0
  MAPE  = mean(abs(residual / actual)[abs(actual) > eps]) * 100,
  # sMAPE: always stable
  sMAPE = mean(200 * abs(residual) / (abs(actual) + abs(forecast) + eps))
), by = model]
print(metrics)

# Plot combined forecasts
ggplot(all_results, aes(x = interval, y = actual)) +
  geom_line(size = 0.6, color = "black") +
  geom_line(aes(y = forecast, color = model), size = 0.6) +
  labs(title = "Model Comparison: Forecast vs Actual",
       x = "Time", y = "Consumption (kWh)") +
  theme_minimal() +
  facet_wrap(~model, ncol = 1)

# Plot residuals comparison
ggplot(all_results, aes(x = interval, y = residual, color = model)) +
  geom_line(alpha = 0.6) +
  labs(title = "Model Residuals Comparison",
       x = "Time", y = "Residual (Actual - Forecast)") +
  theme_minimal() +
  facet_wrap(~model, ncol = 1)

# Create overlaid residuals plot
ggplot(all_results, aes(x = interval, y = residual, color = model)) +
  geom_line(alpha = 0.7, size = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  labs(title = "Residuals Comparison Across Models",
       x = "Time", y = "Residual (Actual - Forecast)",
       color = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Residual diagnostics
for (mod in unique(all_results$model)) {
  cat(sprintf("\n=== Residual diagnostics for %s ===\n", mod))
  res <- all_results[model == mod, residual]
  
  cat(sprintf("Mean: %.4f\n", mean(res)))
  cat(sprintf("Std Dev: %.4f\n", sd(res)))
  cat(sprintf("Skewness: %.4f\n", skewness(res)))
  cat(sprintf("Kurtosis: %.4f\n", kurtosis(res)))
  
  # Normality test
  jb_test <- jarque.bera.test(res)
  cat(sprintf("Jarque-Bera p-value: %.4f\n", jb_test$p.value))
  
  # Autocorrelation test
  lb_test <- Box.test(res, lag = 24, type = "Ljung-Box")
  cat(sprintf("Ljung-Box p-value: %.4f\n", lb_test$p.value))
}
