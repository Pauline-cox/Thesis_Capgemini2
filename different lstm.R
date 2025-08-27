# ================================================================
# LSTM WITH EXOGENOUS REGRESSORS (single input)
# ================================================================

library(data.table)
library(lubridate)
library(keras)
library(ggplot2)
library(moments)
library(tseries)

# ==== 0. SELECT RELEVANT FEATURES ================================
feature_columns <- c(
  # occupancy & indoor environment
  "total_occupancy", "co2", "tempC", "humidity", "sound", "lux",
  
  # external weather
  "global_radiation", "temperature", "wind_speed", "sunshine_minutes",
  
  # calendar
  "office_hours", "is_weekend", "holiday", 
  "hour_sin", "hour_cos", "weekday_sin", "weekday_cos",
  
  # lags & rolling stats
  "lag_24", "lag_72", "lag_168", "lag_336", "lag_504", 
  "rollmean_24", "rollmean_168"
)

# Keep only features that exist
feature_columns <- intersect(feature_columns, names(model_data))

# Ensure categorical / logical columns are numeric
make_numeric <- function(dt, cols) {
  for (col in cols) {
    if (is.logical(dt[[col]]) || is.factor(dt[[col]])) {
      dt[[col]] <- as.numeric(dt[[col]])
    }
  }
}
make_numeric(model_data, feature_columns)

# ==== 1. TRAIN/TEST SPLIT ========================================
train_start <- as.POSIXct("2023-09-01 00:00:00", tz="UTC")
train_end   <- as.POSIXct("2024-09-30 23:00:00", tz="UTC")
test_start  <- as.POSIXct("2024-10-01 00:00:00", tz="UTC")
test_end    <- as.POSIXct("2024-12-31 23:00:00", tz="UTC")

train_dt <- model_data[interval >= train_start & interval <= train_end]
test_dt  <- model_data[interval >= test_start & interval <= test_end]

# ==== 2. MIN-MAX SCALING =========================================
scale_minmax <- function(x, min_, max_) (x - min_) / (max_ - min_ + 1e-6)

# Features
feature_mins <- sapply(train_dt[, ..feature_columns], min, na.rm=TRUE)
feature_maxs <- sapply(train_dt[, ..feature_columns], max, na.rm=TRUE)

X_train_scaled <- sweep(train_dt[, ..feature_columns], 2, feature_mins, "-")
X_train_scaled <- sweep(X_train_scaled, 2, feature_maxs - feature_mins + 1e-6, "/")

X_test_scaled  <- sweep(test_dt[, ..feature_columns], 2, feature_mins, "-")
X_test_scaled  <- sweep(X_test_scaled, 2, feature_maxs - feature_mins + 1e-6, "/")

# Target
y_min <- min(train_dt$total_consumption_kWh, na.rm=TRUE)
y_max <- max(train_dt$total_consumption_kWh, na.rm=TRUE)

y_train_scaled <- scale_minmax(train_dt$total_consumption_kWh, y_min, y_max)
y_test_scaled  <- scale_minmax(test_dt$total_consumption_kWh, y_min, y_max)

invert_y <- function(y_scaled) y_scaled * (y_max - y_min) + y_min

# ==== 3. CREATE LSTM DATASET ====================================
create_lstm_dataset <- function(X, y, lookback = 168, horizon = 24) {
  n_samples <- nrow(X) - lookback - horizon + 1
  X_array <- array(dim = c(n_samples, lookback, ncol(X)))
  y_array <- array(dim = c(n_samples, horizon))
  
  for (i in 1:n_samples) {
    X_array[i,,] <- as.matrix(X[i:(i + lookback - 1), ])
    y_array[i,]  <- y[(i + lookback):(i + lookback + horizon - 1)]
  }
  list(X = X_array, y = y_array)
}

lookback <- 168   # 1 week
horizon  <- 24    # 1 day

train_data <- create_lstm_dataset(X_train_scaled, y_train_scaled, lookback, horizon)
test_data  <- create_lstm_dataset(X_test_scaled,  y_test_scaled,  lookback, horizon)

# ==== 4. BUILD LSTM MODEL =======================================
build_lstm_model <- function(input_shape, horizon) {
  model <- keras_model_sequential() %>%
    layer_lstm(units = 150, input_shape = input_shape, return_sequences = TRUE) %>%
    layer_dropout(rate = 0.3) %>%
    layer_lstm(units = 100, return_sequences = TRUE) %>%
    layer_dropout(rate = 0.3) %>%
    layer_lstm(units = 50, return_sequences = FALSE) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 50, activation = 'relu') %>%
    layer_dense(units = horizon, activation = "linear")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.0005),
    loss = "mse",
    metrics = "mae"
  )
  model
}

model <- build_lstm_model(c(lookback, ncol(X_train_scaled)), horizon)
summary(model)

# ==== 5. TRAIN ==================================================
history <- model %>% fit(
  x = train_data$X, y = train_data$y,
  epochs = 100,
  batch_size = 64,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(
    callback_early_stopping(patience = 10, restore_best_weights = TRUE),
    callback_reduce_lr_on_plateau(factor = 0.5, patience = 5)
  )
)
plot(history)

# ==== 6. ROLLING FORECAST =======================================
lstm_rolling_forecast <- function(model, X_test, y_test, lookback, horizon) {
  n_test <- nrow(X_test)
  forecasts <- numeric(0)
  actuals   <- numeric(0)
  stamps    <- as.POSIXct(character(0))
  
  for (i in seq(1, n_test - lookback - horizon + 1, by = horizon)) {
    X_input <- array_reshape(as.matrix(X_test[i:(i + lookback - 1), ]),
                             c(1, lookback, ncol(X_test)))
    pred_scaled <- predict(model, X_input, verbose = 0)
    
    preds <- invert_y(as.numeric(pred_scaled))
    
    idx <- (i + lookback):(i + lookback + horizon - 1)
    actual <- invert_y(y_test[idx])
    
    forecasts <- c(forecasts, preds)
    actuals   <- c(actuals, actual)
    stamps    <- c(stamps, test_dt$interval[idx])
  }
  data.table(interval=stamps, actual=actuals, forecast=forecasts)
}

res_dt <- lstm_rolling_forecast(
  model,
  as.matrix(X_test_scaled),
  y_test_scaled,
  lookback,
  horizon
)

# ==== 7. EVALUATION =============================================
res_dt[, residual := actual - forecast]
MSE  <- mean(res_dt$residual^2)
RMSE <- sqrt(MSE)
MAE  <- mean(abs(res_dt$residual))
MAPE <- mean(abs(res_dt$residual / pmax(actual, 1e-6))) * 100
sMAPE <- mean(2*abs(residual)/(pmax(abs(actual)+abs(forecast), 1e-6))) * 100

cat("\n==== LSTM FORECAST RESULTS ====\n")
cat(sprintf("MSE   = %.2f\n", MSE))
cat(sprintf("RMSE  = %.2f\n", RMSE))
cat(sprintf("MAE   = %.2f\n", MAE))
cat(sprintf("MAPE  = %.2f%%\n", MAPE))
cat(sprintf("sMAPE = %.2f%%\n", sMAPE))
ls
# ==== 8. VISUALIZATION ==========================================
ggplot(res_dt, aes(x = interval)) +
  geom_line(aes(y = actual, colour = "Actual"), size = 0.5) +
  geom_line(aes(y = forecast, colour = "Forecast"), size = 0.6, linetype="dashed") +
  labs(title = "LSTM Forecast vs Actuals", x = "Time", y = "kWh") +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal()

# Residual plots
ggplot(res_dt, aes(x = interval, y = residual)) +
  geom_line(color="steelblue", alpha=0.7) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  labs(title="LSTM Residuals Over Time", x="Time", y="Residual") +
  theme_minimal()
