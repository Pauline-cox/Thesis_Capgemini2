# --------- GLOBAL SETTINGS ----------
horizon <- 24
start_date <- as.POSIXct("2024-11-15")

# --------- PRE-TUNING: Random Forest & XGBoost ----------
rf_tuned <- caret::train(
  get_model_formula(),
  data = data[1:5000, ],
  method = "rf",
  trControl = caret::trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(mtry = c(2, 4, 6, 8, 10)),
  ntree = 300
)
best_mtry <- rf_tuned$bestTune$mtry
fit_rf <- function(train) {
  randomForest(get_model_formula(), data = train, mtry = best_mtry, ntree = 300)
}

xgb_tuned <- caret::train(
  x = model.matrix(get_model_formula(), data[1:5000, ])[, -1],
  y = data[1:5000, ]$target,
  method = "xgbTree",
  trControl = caret::trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    nrounds = c(100, 150),
    max_depth = c(3, 6),
    eta = c(0.1),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)
best_xgb <- xgb_tuned$bestTune
fit_xgb <- function(train) {
  x <- model.matrix(get_model_formula(), train)[, -1]
  y <- train$target
  xgboost::xgboost(
    data = x,
    label = y,
    nrounds = best_xgb$nrounds,
    max_depth = best_xgb$max_depth,
    eta = best_xgb$eta,
    gamma = best_xgb$gamma,
    colsample_bytree = best_xgb$colsample_bytree,
    min_child_weight = best_xgb$min_child_weight,
    subsample = best_xgb$subsample,
    objective = "reg:squarederror",
    verbose = 0
  )
}

# --------- MODEL FUNCTIONS ----------
fit_sarima <- function(train) {
  y <- ts(train$target, frequency = 168)
  forecast::auto.arima(y, seasonal = TRUE)
}

fit_sarimax <- function(train) {
  xreg <- model.matrix(get_model_formula(), train)[, -1]
  xreg <- xreg[, apply(xreg, 2, var) > 0, drop = FALSE]
  y <- ts(train$target, frequency = 168)
  forecast::auto.arima(y, xreg = xreg, seasonal = TRUE)
}

fit_lstm_cv <- function(train, test) {
  lstm_data <- prepare_lstm(rbind(train, test))
  input_shape <- dim(lstm_data$x_train)[2:3]
  model <- train_lstm(input_shape, lstm_data$x_train, lstm_data$y_train)
  pred <- predict(model, lstm_data$x_test)
  pred * (lstm_data$target_max - lstm_data$target_min) + lstm_data$target_min
}

# --------- CV FUNCTION ----------
expanding_window_cv <- function(data, initial_window, step = 24, model_function, use_lstm = FALSE, verbose = TRUE) {
  n <- nrow(data)
  forecasts <- c()
  actuals <- c()
  times <- c()
  
  dec_indices <- which(data$interval >= start_date)
  indices <- seq(from = min(dec_indices), to = max(dec_indices) - horizon + 1, by = step)
  total_iters <- length(indices)
  
  for (i in seq_along(indices)) {
    start <- indices[i] - 1
    end <- indices[i] + horizon - 1
    if (start < initial_window || end > n) next
    
    if (verbose) cat(sprintf("Iteration %d/%d | Train: 1:%d | Test: %d:%d\n", i, total_iters, start, indices[i], end))
    iter_start <- Sys.time()
    
    train_set <- data[1:start]
    test_set <- data[indices[i]:end]
    
    if (use_lstm) {
      pred <- fit_lstm_cv(train_set, test_set)
    } else {
      model <- model_function(train_set)
      x_test <- if ("xgb.Booster" %in% class(model)) {
        model.matrix(get_model_formula(), test_set)[, -1]
      } else if ("Arima" %in% class(model)) {
        xreg <- model.matrix(get_model_formula(), test_set)[, -1]
        xreg <- xreg[, apply(xreg, 2, var) > 0, drop = FALSE]
        return(as.vector(forecast::forecast(model, xreg = xreg, h = horizon)$mean))
      } else {
        test_set
      }
      pred <- predict(model, x_test)
    }
    
    forecasts <- c(forecasts, as.vector(pred))
    actuals <- c(actuals, test_set$target)
    times <- c(times, test_set$interval)
    
    iter_end <- Sys.time()
    cat(sprintf("Iteration took %.2f seconds\n\n", as.numeric(difftime(iter_end, iter_start, units = "secs"))))
  }
  
  list(pred = forecasts, actual = actuals, time = times)
}

# --------- RUN ALL MODELS ----------
cv_result_rf <- expanding_window_cv(data, initial_window = 5000, step = 24, model_function = fit_rf)
cv_result_xgb <- expanding_window_cv(data, initial_window = 5000, step = 24, model_function = fit_xgb)
cv_result_sarima <- expanding_window_cv(data, initial_window = 5000, step = 24, model_function = fit_sarima)
cv_result_sarimax <- expanding_window_cv(data, initial_window = 5000, step = 24, model_function = fit_sarimax)
cv_result_lstm <- expanding_window_cv(data, initial_window = 5000, step = 24, model_function = NULL, use_lstm = TRUE)

# --------- EVALUATE ---------
models <- list(
  SARIMA = cv_result_sarima$pred,
  SARIMAX = cv_result_sarimax$pred,
  RandomForest = cv_result_rf$pred,
  XGBoost = cv_result_xgb$pred,
  LSTM = cv_result_lstm$pred
)

evaluate_models(models, actual = cv_result_rf$actual, test_time = cv_result_rf$time)
