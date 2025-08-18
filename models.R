
fit_sarima <- function(train) {
  y <- ts(train$target, frequency = 168)
  auto.arima(y, seasonal = TRUE)
}

fit_sarimax <- function(train) {
  xreg <- model.matrix(get_model_formula(), train)[, -1]
  xreg <- xreg[, apply(xreg, 2, function(col) var(col, na.rm = TRUE) > 0), drop = FALSE]
  y <- ts(train$target, frequency = 168)
  auto.arima(y, xreg = xreg, seasonal = TRUE)
}

# fit_rf <- function(train) {
#   randomForest(get_model_formula(), data = train, ntree = 300)
# }
# 
# fit_xgb <- function(train) {
#   x <- model.matrix(get_model_formula(), train)[, -1]
#   y <- train$target
#   xgboost(data = x, label = y, nrounds = 150, objective = "reg:squarederror", verbose = 1)
# }
fit_rf <- function(train) {
  train <- as.data.frame(train)  # Ensure compatibility with caret
  formula <- get_model_formula()
  
  control <- trainControl(method = "cv", number = 5)
  tune_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10))
  
  caret::train(
    formula,
    data = train,
    method = "rf",
    trControl = control,
    tuneGrid = tune_grid,
    ntree = 300
  )
}

fit_xgb <- function(train) {
  train <- as.data.frame(train)
  x <- model.matrix(get_model_formula(), train)[, -1]
  y <- train$target
  
  control <- trainControl(method = "cv", number = 5)
  tune_grid <- expand.grid(
    nrounds = c(100, 150, 200),
    max_depth = c(3, 6, 9),
    eta = c(0.01, 0.1, 0.3),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  caret::train(
    x = x,
    y = y,
    method = "xgbTree",
    trControl = control,
    tuneGrid = tune_grid,
    verbose = TRUE
  )
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
train_lstm <- function(input_shape, x_train, y_train,
                       units1 = 128, units2 = 64,
                       dropout_rate = 0.2,
                       learning_rate = 0.001) {
  
  model <- keras_model_sequential() %>%
    layer_lstm(units = units1, input_shape = input_shape, return_sequences = TRUE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_lstm(units = units2, return_sequences = FALSE) %>%
    layer_dense(units = 1, activation = "linear")
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = learning_rate),
    metrics = "mae"
  )
  
  model %>% fit(
    x_train, y_train,
    epochs = 150,
    batch_size = 32,
    validation_split = 0.2,
    callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
    verbose = 1
  )
  
  return(model)
}

train_lstm <- function(input_shape, x_train, y_train, units = 64, dropout_rate = 0.3, learning_rate = 0.001) {
  model <- keras_model_sequential() %>%
    layer_lstm(units = units, input_shape = input_shape, return_sequences = TRUE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_batch_normalization() %>%
    layer_lstm(units = units, return_sequences = FALSE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 1, activation = "sigmoid") 
  
  
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
fit_sarimax_lstm_hybrid <- function(train, timesteps = 24, epochs = 20, batch_size = 32) {
  # ----- SARIMA with exogenous variables -----
  xreg <- model.matrix(get_model_formula(), train)[, -1]
  xreg <- xreg[, apply(xreg, 2, function(col) var(col, na.rm = TRUE) > 0), drop = FALSE]
  
  sarima <- auto.arima(ts(train$target, frequency = 168), xreg = xreg, seasonal = TRUE)
  
  # ----- Residuals -----
  train[, residual := target - fitted(sarima)]
  train[, residual_lag1 := shift(residual, 1)]
  train[, residual_lag24 := shift(residual, 24)]
  train <- na.omit(train)  # Ensure clean input
  y <- train$residual
  
  # ----- Normalize xreg -----
  x_scaled <- scale(xreg)
  
  # ----- Build LSTM sequences -----
  samples <- nrow(x_scaled) - timesteps
  if (samples <= 0) stop("Not enough samples for given timesteps.")
  
  x_array <- array(NA, dim = c(samples, timesteps, ncol(x_scaled)))
  y_array <- numeric(samples)
  
  for (i in 1:samples) {
    x_array[i,,] <- x_scaled[i:(i + timesteps - 1), ]
    y_array[i] <- y[i + timesteps]
  }
  
  # ----- Train/test split for LSTM -----
  train_idx <- 1:floor(0.8 * samples)
  val_idx <- (floor(0.8 * samples) + 1):samples
  
  # ----- Build and train LSTM model -----
  lstm <- keras_model_sequential() %>%
    layer_lstm(units = 64, input_shape = c(timesteps, ncol(x_scaled)), return_sequences = TRUE) %>%
    layer_dropout(rate = 0.3) %>%
    layer_lstm(units = 32) %>%
    layer_dense(units = 1, activation = "linear")
  
  lstm %>% compile(loss = "mse", optimizer = "adam", metrics = "mae")
  
  lstm %>% fit(
    x = x_array[train_idx,,],
    y = y_array[train_idx],
    validation_data = list(x_array[val_idx,,], y_array[val_idx]),
    epochs = epochs,
    batch_size = batch_size,
    verbose = 1
  )
  
  # ----- Return model components -----
  list(
    sarima = sarima,
    lstm = lstm,
    center = attr(x_scaled, "scaled:center"),
    scale = attr(x_scaled, "scaled:scale"),
    timesteps = timesteps
  )
}

predict_sarimax_lstm <- function(model, test) {
  xreg <- model.matrix(get_model_formula(), test)[, -1]
  xreg <- xreg[, apply(xreg, 2, function(col) var(col, na.rm = TRUE) > 0), drop = FALSE]
  xreg <- cbind(xreg,
                residual_lag1 = test$residual_lag1,
                residual_lag24 = test$residual_lag24)
  
  # Base SARIMA forecast
  sarima_pred <- forecast(model$sarima, xreg = xreg)$mean
  
  # LSTM residual forecast
  x_scaled <- scale(xreg, center = model$center, scale = model$scale)
  n <- nrow(x_scaled)
  ts <- model$timesteps
  
  if (n < ts) stop("Not enough test rows for residual LSTM prediction.")
  
  x_array <- array(NA, dim = c(n - ts + 1, ts, ncol(x_scaled)))
  
  for (i in 1:(n - ts + 1)) {
    x_array[i,,] <- x_scaled[i:(i + ts - 1), ]
  }
  
  lstm_pred <- as.vector(predict(model$lstm, x_array))
  
  # Align SARIMA prediction to residual length
  sarima_pred_trimmed <- sarima_pred[model$timesteps:length(sarima_pred)]
  
  # Combine SARIMA + residuals
  combined <- sarima_pred_trimmed + lstm_pred
  return(combined)
}


