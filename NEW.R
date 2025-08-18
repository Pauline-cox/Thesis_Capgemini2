# ==================== Environment Setup ====================
initialize_environment()
raw_data <- load_and_prepare_data()
model_data <- prepare_features(raw_data)

# ================== Model Functions ==================
get_model_formula <- function() {
  as.formula(
    target ~ tempC + humidity + co2 + sound + lux + 
      HH + U + Q + FH + SQ + #M + T +
      # is_maandag + is_dinsdag + is_woensdag + is_donderdag + is_vrijdag + is_zaterdag +
      weekday +
      total_occupancy + office_hours + holiday + #is_weekend +
      lag_24 + lag_48 + lag_168 + lag_336 + lag_504 +
      rollmean_24 + rollmean_168 
  )
}

fit_sarima <- function(train) {
  y <- ts(train$target, frequency = 168)
  auto.arima(y, seasonal = TRUE)
}


fit_sarimax <- function(train) {
  xreg <- model.matrix(get_model_formula(), train)[, -1]
  xreg <- xreg[, apply(xreg, 2, function(col) var(col, na.rm = TRUE) > 0), drop = FALSE]  # remove zero-variance columns
  y <- ts(train$target, frequency = 168)
  auto.arima(y, xreg = xreg, seasonal = TRUE)
}

fit_rf <- function(train) {
  randomForest(get_model_formula(), data = train, ntree = 300)
}

fit_xgb <- function(train) {
  x <- model.matrix(get_model_formula(), train)[, -1]
  y <- train$target
  xgboost(data = x, label = y, nrounds = 150, objective = "reg:squarederror", verbose = 1)
}

# ================== LSTM Functions ==================
prepare_lstm <- function(data, timesteps = 168) {
  data <- as.data.frame(data)
  
  # Define training index BEFORE preprocessing
  samples <- nrow(data) - timesteps
  train_idx <- 1:floor(0.8 * samples)
  
  # Store original target values for proper evaluation
  original_target <- data$target
  
  # Recipe is fit only on training data
  rec <- recipe(get_model_formula(), data = data[train_idx, ]) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_range(all_numeric(), -all_outcomes(), min = 0, max = 1) %>%
    prep()
  
  norm <- bake(rec, data)
  x <- as.matrix(norm[, setdiff(names(norm), "target")])
  
  # Store target scaling parameters
  target_min <- min(original_target[train_idx], na.rm = TRUE)
  target_max <- max(original_target[train_idx], na.rm = TRUE)
  
  # Manually normalize target
  y_normalized <- (original_target - target_min) / (target_max - target_min)
  
  # Create sequences
  x_array <- array(NA, dim = c(samples, timesteps, ncol(x)))
  y_vec <- numeric(samples)
  y_original_vec <- numeric(samples)
  
  for (i in 1:samples) {
    x_array[i, , ] <- x[i:(i + timesteps - 1), ]
    y_vec[i] <- y_normalized[i + timesteps]
    y_original_vec[i] <- original_target[i + timesteps]
  }
  
  # Split into train/test
  test_idx <- (length(train_idx) + 1):samples
  
  list(
    x_train = x_array[train_idx, , , drop = FALSE],
    y_train = y_vec[train_idx],
    x_test = x_array[test_idx, , , drop = FALSE],
    y_test = y_vec[test_idx],
    # Store original scale values for proper evaluation
    y_test_original = y_original_vec[test_idx],
    recipe = rec,
    target_min = target_min,
    target_max = target_max
  )
}
train_lstm <- function(input_shape, x_train, y_train, units = 64, dropout_rate = 0.3, learning_rate = 0.001) {
  model <- keras_model_sequential() %>%
    layer_lstm(units = units, input_shape = input_shape, return_sequences = TRUE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_batch_normalization() %>%
    layer_lstm(units = units, return_sequences = FALSE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 1, activation = "sigmoid")  # <-- Add this

  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = learning_rate),
    metrics = c("mae")
  )
  
  early_stop <- callback_early_stopping(
    monitor = "val_loss",
    patience = 5,
    min_delta = 1e-5,  
    restore_best_weights = TRUE,
    
  )
  
  model %>% fit(
    x_train, y_train,
    epochs = 100,
    batch_size = 32,
    validation_split = 0.2,
    callbacks = list(early_stop),
    verbose = 1
  )
  
  return(model)
}
# ================== Hybrid SARIMAX + LSTM ==================
fit_sarimax_lstm_hybrid <- function(train) {
  # Use model formula like SARIMAX does
  xreg <- model.matrix(get_model_formula(), train)[, -1]
  
  # Fit SARIMA with exogenous regressors
  sarima <- auto.arima(ts(train$target, frequency = 168), xreg = xreg, seasonal = TRUE)
  
  # Residuals for LSTM
  train[, residual := target - fitted(sarima)]
  
  # Normalize residuals inputs (same xreg used for SARIMA)
  x_scaled <- scale(xreg)
  y <- train$residual
  
  # LSTM input: [samples, timesteps, features]
  timesteps <- 1  # Simple 1-step LSTM for residual modeling
  x_array <- array(x_scaled, dim = c(nrow(x_scaled), timesteps, ncol(x_scaled)))
  
  # LSTM model
  lstm <- keras_model_sequential() %>%
    layer_lstm(units = 32, input_shape = c(timesteps, ncol(x_scaled))) %>%
    layer_dense(units = 1)
  
  lstm %>% compile(loss = "mse", optimizer = "adam")
  lstm %>% fit(x_array, y, epochs = 20, batch_size = 32, verbose = 0)
  
  list(
    sarima = sarima,
    lstm = lstm,
    center = attr(x_scaled, "scaled:center"),
    scale = attr(x_scaled, "scaled:scale")
  )
}
predict_sarimax_lstm <- function(model, test) {
  xreg <- model.matrix(get_model_formula(), test)[, -1]
  
  # Base SARIMA forecast
  sarima_pred <- forecast(model$sarima, xreg = xreg)$mean
  
  # LSTM residual forecast
  x_scaled <- scale(xreg, center = model$center, scale = model$scale)
  x_array <- array(x_scaled, dim = c(nrow(x_scaled), 1, ncol(x_scaled)))
  lstm_pred <- as.vector(predict(model$lstm, x_array))
  
  # Combine SARIMA + residuals
  as.vector(sarima_pred) + lstm_pred
}


# ================== Run Forecast ==================
horizon <- 1
data <- prepare_data(model_data, horizon)
sets <- split_data(data)
train <- sets$train
test <- sets$test
actual <- test$target


# SARIMA
sarima_model <- fit_sarima(train)
pred_sarima <- forecast(sarima_model, h = nrow(test))$mean

# SARIMAX
sarimax_model <- fit_sarimax(train)
x_test <- model.matrix(get_model_formula(), test)[, -1]
pred_sarimax <- forecast(sarimax_model, xreg = x_test)$mean

# RF
rf_model <- fit_rf(train)
pred_rf <- predict(rf_model, test)

# XGBoost
xgb_model <- fit_xgb(train)
x_test <- model.matrix(get_model_formula(), test)[, -1]
pred_xgb <- predict(xgb_model, x_test)

# LSTM
lstm_data <- prepare_lstm(data)
input_shape <- dim(lstm_data$x_train)[2:3]
lstm_model <- train_lstm(input_shape, lstm_data$x_train, lstm_data$y_train)
pred_lstm_normalized <- as.vector(predict(lstm_model, lstm_data$x_test))
pred_lstm <- pred_lstm_normalized * (lstm_data$target_max - lstm_data$target_min) + lstm_data$target_min

# Hybrid
hybrid_model <- fit_sarimax_lstm_hybrid(train)
pred_hybrid <- predict_sarimax_lstm(hybrid_model, test)

# Collect Predictions
models <- list(
  SARIMA = pred_sarima,
  SARIMAX = pred_sarimax,
  RandomForest = pred_rf,
  XGBoost = pred_xgb,
  LSTM = pred_lstm,
  LSTM2 = pred_lstm2,
  Hybrid = pred_hybrid
)

# Collect Predictions
models <- list(LSTM = pred_lstm,LSTM2 = pred_lstm2)
models <- list(RF = pred_rf, XG = pred_xgb, SaRIMA = pred_sarima)


evaluate_models(models, actual, test$interval, lstm_data)  

# === Plot each model separately ===
for (model_name in names(models)) {
  pred <- models[[model_name]]
  
  # Use original scale for LSTM and LSTM_resc
  actual_vec <- if (model_name %in% c("LSTM", "LSTM_resc")) {
    lstm_data$y_test_original
  } else {
    actual
  }
  
  # Align lengths
  n <- min(length(pred), length(actual_vec))
  time_vals <- tail(test$interval, n)
  
  plot_df <- data.table(
    Time = time_vals,
    Actual = tail(actual_vec, n),
    Prediction = tail(pred, n)
  )
  
  p <- ggplot(plot_df, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1, linetype = "solid") +
    geom_line(aes(y = Prediction), color = "blue", size = 1, linetype = "dashed") +
    labs(
      title = paste(model_name, "- Forecast vs Actual"),
      x = "Time",
      y = "Energy Consumption (kWh)"
    ) +
    theme_minimal()
  
  print(p)
}

