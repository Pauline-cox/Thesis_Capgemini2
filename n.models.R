# R/40_models.R
# -----------------------------------------------------------------------------
# Model-specific fit/predict functions + dynamic formula selection.
# -----------------------------------------------------------------------------

get_formula_for_model <- function(model_name, selected) {
  if (model_name == "SARIMA") return(as.formula("target ~ 1"))
  feats <- switch(model_name,
                  SARIMAX = selected$features_for_SARIMAX,
                  LSTM    = selected$features_for_LSTM,
                  Hybrid  = selected$features_for_LSTM,  # not used directly but kept for symmetry
                  selected$features_for_TREES)
  build_formula("target", feats)
}

# ---- SARIMA / SARIMAX ----
fit_sarima <- function(train) {
  y <- ts(train$target, frequency = 168)
  auto.arima(y, seasonal = TRUE)
}

fit_sarimax <- function(train, sel) {
  xreg <- model.matrix(get_formula_for_model("SARIMAX", sel), train)[, -1]
  xreg <- xreg[, apply(xreg, 2, var, na.rm = TRUE) > 0, drop = FALSE]
  y <- ts(train$target, frequency = 168)
  auto.arima(y, xreg = xreg, seasonal = TRUE)
}

# ---- RF / XGB ----
fit_rf <- function(train, sel) {
  caret::train(
    get_formula_for_model("RandomForest", sel),
    data = as.data.frame(train),
    method = "rf",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = expand.grid(mtry = c(2,4,6,8,10)),
    ntree = 300
  )
}

fit_xgb <- function(train, sel) {
  x <- model.matrix(get_formula_for_model("XGBoost", sel), as.data.frame(train))[, -1]
  y <- train$target
  caret::train(
    x = x, y = y,
    method = "xgbTree",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = expand.grid(
      nrounds = c(100,150),
      max_depth = c(3,6),
      eta = c(0.01, 0.1),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    ),
    verbose = TRUE
  )
}

# ---- LSTM ----
prepare_lstm <- function(data, sel, timesteps = 168) {
  data <- as.data.frame(data)
  
  # 1) predictors from dynamic formula (not the formula itself)
  lstm_form <- get_formula_for_model("LSTM", sel)
  preds <- predictors_from_formula(lstm_form)
  
  # 2) remove datetime columns from predictors
  is_datetime <- vapply(data, function(x) inherits(x, c("POSIXct","POSIXt","Date")), logical(1))
  preds <- setdiff(preds, names(is_datetime)[is_datetime])
  
  # 3) keep only existing predictors
  preds <- intersect(preds, names(data))
  if (!length(preds)) stop("No valid predictors for LSTM after filtering.")
  
  # 4) clean formula for recipe
  lstm_form_clean <- build_formula("target", preds)
  
  # 5) indices
  samples <- nrow(data) - timesteps
  if (samples <= 0) stop("Not enough rows for the chosen timesteps in LSTM.")
  train_idx <- 1:floor(0.8 * samples)
  original_target <- data$target
  
  # 6) recipe on training only
  rec <- recipe(lstm_form_clean, data = data[train_idx, , drop = FALSE]) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_range(all_numeric(), -all_outcomes(), min = 0, max = 1) %>%
    prep()
  
  norm <- bake(rec, data)
  
  # 7) ensure numeric predictors
  predictors <- setdiff(names(norm), "target")
  norm[predictors] <- lapply(norm[predictors], function(z) as.numeric(as.character(z)))
  if (any(vapply(norm[predictors], function(z) !is.numeric(z), logical(1))))
    stop("Non-numeric predictors detected after preprocessing for LSTM.")
  
  x <- as.matrix(norm[, predictors, drop = FALSE])
  
  # 8) scale target using train range
  target_min <- min(original_target[train_idx], na.rm = TRUE)
  target_max <- max(original_target[train_idx], na.rm = TRUE)
  y_normalized <- (original_target - target_min) / (target_max - target_min)
  
  # 9) sequences
  x_array <- array(NA_real_, dim = c(samples, timesteps, ncol(x)))
  y_vec <- numeric(samples)
  y_original_vec <- numeric(samples)
  
  for (i in 1:samples) {
    x_array[i, , ] <- x[i:(i + timesteps - 1), , drop = FALSE]
    y_vec[i] <- y_normalized[i + timesteps]
    y_original_vec[i] <- original_target[i + timesteps]
  }
  
  test_idx <- (length(train_idx) + 1):samples
  
  list(
    x_train = x_array[train_idx, , , drop = FALSE],
    y_train = y_vec[train_idx],
    x_test  = x_array[test_idx, , , drop = FALSE],
    y_test  = y_vec[test_idx],
    y_test_original = y_original_vec[test_idx],
    target_min = target_min,
    target_max = target_max
  )
}

train_lstm <- function(input_shape, x_train, y_train,
                       units1 = 128, units2 = 64,
                       dropout_rate = 0.2, learning_rate = 0.001) {
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
  model
}

# =============================
# Hybrid residual models (SARIMA → LSTM) and (SARIMAX → LSTM)
# Add to n.models.R (requires keras, recipes, forecast, data.table)
# =============================

# Helper: build residual LSTM training objects using the existing dynamic feature selection
.prepare_residual_lstm <- function(train, test = NULL, sel, timesteps = 168, feature_model = "LSTM") {
  requireNamespace("recipes")
  requireNamespace("data.table")
  
  # 1) choose predictor set (reuse your dynamic feature machinery)
  lstm_form <- get_formula_for_model(feature_model, sel)
  preds <- predictors_from_formula(lstm_form)
  
  # 2) drop datetime columns from predictors
  is_datetime <- vapply(train, function(x) inherits(x, c("POSIXct","POSIXt","Date")), logical(1))
  preds <- setdiff(preds, names(is_datetime)[is_datetime])
  
  # 3) keep only existing predictors
  preds <- intersect(preds, names(train))
  if (!length(preds)) stop("No valid predictors for residual LSTM after filtering.")
  
  # 4) make a clean formula with residual as outcome
  res_form <- build_formula("residual", preds)
  
  # 5) recipe: one-hot + range scaling on TRAIN ONLY
  rec <- recipe(res_form, data = train) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_range(all_numeric(), -all_outcomes(), min = 0, max = 1) %>%
    prep()
  
  # 6) build combined data for sequence windows that span the train→test boundary
  if (is.null(test)) {
    combined <- train
    n_train  <- nrow(train)
  } else {
    combined <- data.table::rbindlist(list(train, test), use.names = TRUE, fill = TRUE)
    n_train  <- nrow(train)
  }
  
  # 7) bake predictors for combined; outcome not needed in predictors matrix
  baked <- bake(rec, combined)
  predictor_cols <- setdiff(names(baked), "residual")
  x_mat <- as.matrix(baked[, predictor_cols, drop = FALSE])
  
  # 8) residual scaling params from TRAIN ONLY
  res_min <- min(train$residual, na.rm = TRUE)
  res_max <- max(train$residual, na.rm = TRUE)
  if (!is.finite(res_min) || !is.finite(res_max) || res_max == res_min) {
    stop("Residuals have zero or invalid range; cannot scale.")
  }
  
  # 9) build TRAIN sequences (windows that end within train)
  samples_train <- n_train - timesteps
  if (samples_train <= 0) stop("Not enough rows for chosen timesteps in residual LSTM (train).")
  
  x_train <- array(NA_real_, dim = c(samples_train, timesteps, ncol(x_mat)))
  y_train <- numeric(samples_train)
  for (i in seq_len(samples_train)) {
    x_train[i,,] <- x_mat[i:(i + timesteps - 1), , drop = FALSE]
    # next-step residual target (scaled 0–1 using TRAIN range)
    y_train[i] <- (train$residual[i + timesteps] - res_min) / (res_max - res_min)
  }
  
  # 10) build PRED sequences (windows that end in test)
  x_pred <- NULL
  pred_len <- 0
  if (!is.null(test)) {
    # For the first test point at index n_train + 1, the window should be
    # (n_train - timesteps + 1) : n_train.
    # For the last test point at index n_train + n_test, the window should be
    # (n_train + n_test - timesteps) : (n_train + n_test - 1).
    # Therefore, i ranges from (n_train - timesteps + 1) to (nrow(combined) - timesteps).
    start_idx <- n_train - timesteps + 1
    end_idx   <- nrow(combined) - timesteps
    pred_len  <- max(0, end_idx - start_idx + 1)  # equals nrow(test)
    if (pred_len > 0) {
      x_pred <- array(NA_real_, dim = c(pred_len, timesteps, ncol(x_mat)))
      j <- 1
      for (i in start_idx:end_idx) {
        x_pred[j,,] <- x_mat[i:(i + timesteps - 1), , drop = FALSE]
        j <- j + 1
      }
    }
  }
  
  list(
    rec = rec,
    predictor_cols = predictor_cols,
    x_train = x_train,
    y_train = y_train,
    x_pred  = x_pred,
    res_min = res_min,
    res_max = res_max,
    timesteps = timesteps,
    input_shape = c(timesteps, ncol(x_mat))
  )
}

# -----------------------------
# HYBRID 1: SARIMA + LSTM (residuals)
# -----------------------------
fit_hybrid_sarima_lstm <- function(train, sel, timesteps = 168,
                                   units1 = 128, units2 = 64,
                                   dropout_rate = 0.2, learning_rate = 0.001,
                                   epochs = 150, batch_size = 32) {
  # Base SARIMA (no exogenous)
  y <- ts(train$target, frequency = 168)
  sarima_model <- auto.arima(y, seasonal = TRUE)
  
  # Residuals on TRAIN
  train$residual <- train$target - as.numeric(fitted(sarima_model))
  
  # Prepare residual LSTM data using your LSTM feature set
  prep <- .prepare_residual_lstm(train = train, sel = sel, timesteps = timesteps, feature_model = "LSTM")
  
  # Build & train LSTM
  lstm_model <- keras_model_sequential() %>%
    layer_lstm(units = units1, input_shape = prep$input_shape, return_sequences = TRUE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_lstm(units = units2, return_sequences = FALSE) %>%
    layer_dense(units = 1, activation = "linear")
  
  lstm_model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = learning_rate),
    metrics = "mae"
  )
  
  lstm_model %>% fit(
    x = prep$x_train, y = prep$y_train,
    epochs = epochs, batch_size = batch_size,
    validation_split = 0.2,
    callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
    verbose = 1
  )
  
  list(
    type = "SARIMA_LSTM",
    sarima = sarima_model,
    lstm   = lstm_model,
    rec    = prep$rec,
    predictor_cols = prep$predictor_cols,
    timesteps = timesteps,
    res_min = prep$res_min,
    res_max = prep$res_max
  )
}

predict_hybrid_sarima_lstm <- function(model, train, test, sel) {
  # 1) Base SARIMA forecast
  base_pred <- forecast(model$sarima, h = nrow(test))$mean
  
  # 2) Recompute TRAIN residuals used for scaling
  train$residual <- train$target - as.numeric(fitted(model$sarima))
  
  # 3) Recreate residual-LSTM predictor windows across train→test boundary
  prep <- .prepare_residual_lstm(train = train, test = test, sel = sel, timesteps = model$timesteps, feature_model = "LSTM")
  if (is.null(prep$x_pred) || dim(prep$x_pred)[1] != nrow(test)) {
    stop("Residual LSTM prediction windows misaligned with test size.")
  }
  
  # 4) Predict residuals (denormalize)
  res_pred_scaled <- as.vector(predict(model$lstm, prep$x_pred))
  res_pred <- res_pred_scaled * (model$res_max - model$res_min) + model$res_min
  
  # 5) Combine base + residuals
  as.numeric(base_pred) + res_pred
}

# -----------------------------
# HYBRID 2: SARIMAX + LSTM (residuals)
# -----------------------------
fit_hybrid_sarimax_lstm <- function(train, sel, timesteps = 168,
                                    units1 = 128, units2 = 64,
                                    dropout_rate = 0.2, learning_rate = 0.001,
                                    epochs = 150, batch_size = 32) {
  # Base SARIMAX with your selected exogenous variables
  xreg_train <- model.matrix(get_formula_for_model("SARIMAX", sel), train)[, -1]
  # Drop constant/degenerate columns
  keep <- apply(xreg_train, 2, function(col) var(col, na.rm = TRUE) > 0)
  if (!any(keep)) stop("All xreg columns are degenerate for SARIMAX.")
  xreg_train <- xreg_train[, keep, drop = FALSE]
  
  y <- ts(train$target, frequency = 168)
  sarimax_model <- auto.arima(y, xreg = xreg_train, seasonal = TRUE)
  
  # Residuals on TRAIN
  train$residual <- train$target - as.numeric(fitted(sarimax_model))
  
  # Prepare residual LSTM data (we still use the LSTM feature set; change feature_model to "SARIMAX" if you prefer)
  prep <- .prepare_residual_lstm(train = train, sel = sel, timesteps = timesteps, feature_model = "LSTM")
  
  # Build & train LSTM
  lstm_model <- keras_model_sequential() %>%
    layer_lstm(units = units1, input_shape = prep$input_shape, return_sequences = TRUE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_lstm(units = units2, return_sequences = FALSE) %>%
    layer_dense(units = 1, activation = "linear")
  
  lstm_model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = learning_rate),
    metrics = "mae"
  )
  
  lstm_model %>% fit(
    x = prep$x_train, y = prep$y_train,
    epochs = epochs, batch_size = batch_size,
    validation_split = 0.2,
    callbacks = list(callback_early_stopping(patience = 10, restore_best_weights = TRUE)),
    verbose = 1
  )
  
  list(
    type = "SARIMAX_LSTM",
    sarimax = sarimax_model,
    lstm    = lstm_model,
    rec     = prep$rec,
    predictor_cols = prep$predictor_cols,
    timesteps = timesteps,
    res_min = prep$res_min,
    res_max = prep$res_max
  )
}

predict_hybrid_sarimax_lstm <- function(model, train, test, sel) {
  # 1) Base SARIMAX forecast for TEST using the SAME xreg definition
  xreg_test <- model.matrix(get_formula_for_model("SARIMAX", sel), test)[, -1]
  # Align columns to training xreg used by auto.arima (drop any new degenerate columns if present)
  # Note: forecast() will internally check colnames; we assume consistency with fit.
  base_pred <- forecast(model$sarimax, xreg = xreg_test)$mean
  
  # 2) Recompute TRAIN residuals used for scaling
  train$residual <- train$target - as.numeric(fitted(model$sarimax))
  
  # 3) Residual-LSTM predictor windows
  prep <- .prepare_residual_lstm(train = train, test = test, sel = sel, timesteps = model$timesteps, feature_model = "LSTM")
  if (is.null(prep$x_pred) || dim(prep$x_pred)[1] != nrow(test)) {
    stop("Residual LSTM prediction windows misaligned with test size.")
  }
  
  # 4) Predict residuals (denormalize)
  res_pred_scaled <- as.vector(predict(model$lstm, prep$x_pred))
  res_pred <- res_pred_scaled * (model$res_max - model$res_min) + model$res_min
  
  # 5) Combine base + residuals
  as.numeric(base_pred) + res_pred
}

