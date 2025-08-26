library(data.table)
library(lubridate)
library(keras)
library(ggplot2)

# ==== 0. SELECT RELEVANT FEATURES ============================================
# Target: total_consumption_kWh
feature_columns <- c(
  # occupancy & indoor environment
  "total_occupancy", "co2", 
  
  # external weather
  "global_radiation", "temperature",
  
  # calendar
  "office_hours", "is_weekend", "holiday", 
  "hour_sin", "hour_cos", "weekday_sin", "weekday_cos",
  
  # lags & rolling stats
  "lag_24", "lag_168", "rollmean_24", "rollmean_168", "lag_504"
)

feature_columns <- intersect(feature_columns, names(model_data))  # keep only existing
if ("month" %in% names(train_dt)) {
  train_dt[, month := as.integer(month)]   # 1–12
  test_dt[,  month := as.integer(month)]
}

# ==== 1. TRAIN/TEST SPLIT ====================================================
train_start <- as.POSIXct("2023-05-01 00:00:00", tz="UTC")
train_end   <- as.POSIXct("2024-09-30 23:00:00", tz="UTC")
test_start  <- as.POSIXct("2024-10-01 00:00:00", tz="UTC")
test_end    <- as.POSIXct("2024-10-31 23:00:00", tz="UTC")

train_dt <- model_data[interval >= train_start & interval <= train_end]
test_dt  <- model_data[interval >= test_start & interval <= test_end]

# ==== 2. MIN-MAX SCALING =====================================================
scale_minmax <- function(x, min_, max_) (x - min_) / (max_ - min_ + 1e-6)

# --- Features ---
feature_mins <- sapply(train_dt[, ..feature_columns], min, na.rm=TRUE)
feature_maxs <- sapply(train_dt[, ..feature_columns], max, na.rm=TRUE)

X_train_scaled <- sweep(train_dt[, ..feature_columns], 2, feature_mins, "-")
X_train_scaled <- sweep(X_train_scaled, 2, feature_maxs - feature_mins + 1e-6, "/")

X_test_scaled  <- sweep(test_dt[, ..feature_columns], 2, feature_mins, "-")
X_test_scaled  <- sweep(X_test_scaled, 2, feature_maxs - feature_mins + 1e-6, "/")

# --- Target ---
y_min <- min(train_dt$total_consumption_kWh, na.rm=TRUE)
y_max <- max(train_dt$total_consumption_kWh, na.rm=TRUE)

y_train_scaled <- scale_minmax(train_dt$total_consumption_kWh, y_min, y_max)
y_test_scaled  <- scale_minmax(test_dt$total_consumption_kWh, y_min, y_max)

invert_y <- function(y_scaled) y_scaled * (y_max - y_min) + y_min

# ==== 3. CREATE LSTM DATASET =================================================
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
horizon  <- 24    # 1 day ahead

train_data <- create_lstm_dataset(X_train_scaled, y_train_scaled, lookback, horizon)
test_data  <- create_lstm_dataset(X_test_scaled, y_test_scaled, lookback, horizon)

# ==== 4. BUILD LSTM MODEL ====================================================
# build_lstm_model <- function(input_shape, horizon) {
#   model <- keras_model_sequential() %>%
#     layer_lstm(units = 128, input_shape = input_shape, return_sequences = TRUE) %>%
#     layer_dropout(rate = 0.2) %>%
#     layer_lstm(units = 64, return_sequences = FALSE) %>%
#     layer_dense(units = horizon, activation = "linear")
#   
#   model %>% compile(
#     optimizer = optimizer_adam(learning_rate = 0.001),
#     loss = "mse",
#     metrics = "mae"
#   )
#   model
# }

# build_lstm_model <- function(input_shape, horizon) {
#   model <- keras_model_sequential() %>%
#     layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu",
#                   input_shape = input_shape, padding = "causal") %>%
#     layer_lstm(units = 128, return_sequences = TRUE) %>%
#     layer_dropout(rate = 0.2) %>%
#     layer_lstm(units = 64, return_sequences = FALSE) %>%
#     layer_dense(units = horizon, activation = "linear")
#   
#   model %>% compile(
#     optimizer = optimizer_adam(learning_rate = 0.001),
#     loss = "mse",
#     metrics = "mae"
#   )
#   model
# }
build_lstm_model <- function(input_shape, horizon) {
  model <- keras_model_sequential() %>%
    layer_lstm(units = 150, input_shape = input_shape, return_sequences = TRUE) %>%
    layer_dropout(rate = 0.3) %>%
    layer_lstm(units = 100, return_sequences = TRUE) %>% # Another layer!
    layer_dropout(rate = 0.3) %>%
    layer_lstm(units = 50, return_sequences = FALSE) %>% # Final LSTM layer
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 50, activation = 'relu') %>%     # Additional Dense layer
    layer_dense(units = horizon, activation = "linear")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.0005), # Lower learning rate
    loss = "mse",
    metrics = "mae"
  )
  model
}

model <- build_lstm_model(c(lookback, ncol(X_train_scaled)), horizon)
summary(model)

# ==== 5. TRAIN ===============================================================
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

# ==== 6. FORECAST (rolling) ==================================================
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

# ==== 7. EVALUATION ==========================================================
res_dt[, residual := actual - forecast]
MSE  <- mean(res_dt$residual^2)
RMSE <- sqrt(MSE)
MAE  <- mean(abs(res_dt$residual))
MAPE <- mean(abs(res_dt$residual / res_dt$actual)) * 100
sMAPE <- mean(2*abs(res_dt$residual)/(abs(res_dt$actual)+abs(res_dt$forecast))) * 100

cat("\n==== LSTM FORECAST RESULTS ====\n")
cat(sprintf("MSE   = %.2f\n", MSE))
cat(sprintf("RMSE  = %.2f\n", RMSE))
cat(sprintf("MAE   = %.2f\n", MAE))
cat(sprintf("MAPE  = %.2f%%\n", MAPE))
cat(sprintf("sMAPE = %.2f%%\n", sMAPE))

# ==== 8. VISUALIZATION =======================================================
ggplot(res_dt, aes(x = interval)) +
  geom_line(aes(y = actual, colour="Actual"), size=0.6) +
  geom_line(aes(y = forecast, colour="Forecast"), size=0.6, alpha=0.8) +
  labs(title="LSTM Forecast vs Actuals",
       subtitle=sprintf("Lookback=%dh, Horizon=%dh", lookback, horizon),
       x="Time", y="kWh") +
  scale_colour_manual(values=c("Actual"="black", "Forecast"="red")) +
  theme_minimal()

