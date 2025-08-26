library(purrr)

# ==== GRID SEARCH FUNCTION ===================================================
run_lstm_experiment <- function(lookback, units, dropout_rate, batch_size, learning_rate,
                                X_train, y_train, X_test, y_test, horizon) {
  
  # Recreate dataset for given lookback
  train_data <- create_lstm_dataset(X_train, y_train, lookback, horizon)
  test_data  <- create_lstm_dataset(X_test,  y_test,  lookback, horizon)
  
  # Build model
  model <- keras_model_sequential() %>%
    layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu",
                  input_shape = c(lookback, ncol(X_train)), padding = "causal") %>%
    layer_lstm(units = units, return_sequences = TRUE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_lstm(units = units/2, return_sequences = FALSE) %>%
    layer_dense(units = horizon, activation = "linear")
  
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = "mse",
    metrics = "mae"
  )
  
  # Train
  history <- model %>% fit(
    x = train_data$X, y = train_data$y,
    epochs = 50,                        # keep lower for grid search
    batch_size = batch_size,
    validation_split = 0.2,
    verbose = 0,
    callbacks = list(
      callback_early_stopping(patience = 5, restore_best_weights = TRUE),
      callback_reduce_lr_on_plateau(factor = 0.5, patience = 3)
    )
  )
  
  # Forecast
  res_dt <- lstm_rolling_forecast(model, as.matrix(X_test), y_test, lookback, horizon)
  res_dt[, residual := actual - forecast]
  
  # Metrics
  MSE  <- mean(res_dt$residual^2)
  RMSE <- sqrt(MSE)
  MAE  <- mean(abs(res_dt$residual))
  MAPE <- mean(abs(res_dt$residual / res_dt$actual)) * 100
  sMAPE <- mean(2*abs(res_dt$residual)/(abs(res_dt$actual)+abs(res_dt$forecast))) * 100
  
  list(
    params = list(lookback=lookback, units=units, dropout=dropout_rate,
                  batch=batch_size, lr=learning_rate),
    metrics = list(MSE=MSE, RMSE=RMSE, MAE=MAE, MAPE=MAPE, sMAPE=sMAPE)
  )
}

# ==== PARAMETER GRID =========================================================
lookbacks <- c(168, 672)
units <- c(64, 128)
dropouts <- c(0.1, 0.2)
batches <- c(32, 64)
lrs <- c(0.001, 0.01)


param_grid <- expand.grid(lookbacks, units, dropouts, batches, lrs,
                          stringsAsFactors = FALSE)

names(param_grid) <- c("lookback", "units", "dropout", "batch", "lr")

# ==== RUN GRID SEARCH (can be long!) =========================================
results <- list()

for (i in 1:nrow(param_grid)) {
  p <- param_grid[i, ]
  cat(sprintf("\n>>> Running config %d/%d: lookback=%d, units=%d, dropout=%.2f, batch=%d, lr=%.4f\n",
              i, nrow(param_grid), p$lookback, p$units, p$dropout, p$batch, p$lr))
  
  res <- run_lstm_experiment(
    lookback = p$lookback, units = p$units,
    dropout_rate = p$dropout, batch_size = p$batch, learning_rate = p$lr,
    X_train = X_train_scaled, y_train = y_train_scaled,
    X_test  = X_test_scaled,  y_test  = y_test_scaled,
    horizon = 24
  )
  
  results[[i]] <- c(res$params, res$metrics)
}

results_dt <- rbindlist(results, fill=TRUE)

# ==== SORT & DISPLAY BEST ====================================================
results_dt <- results_dt[order(sMAPE)]
print((results_dt))

