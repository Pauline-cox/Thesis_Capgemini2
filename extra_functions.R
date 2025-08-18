# ================== Evaluation Functions ==================
safe_mape <- function(actual, predicted) {
  nonzero <- actual != 0
  if (sum(nonzero) == 0) return(NA)
  mean(abs((actual[nonzero] - predicted[nonzero]) / actual[nonzero])) * 100
}
evaluate_model <- function(actual, predicted) {
  list(
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAPE = safe_mape(actual, predicted),
    R2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
}
evaluate_models <- function(models, actual, test_time, lstm_data = NULL) {
  evals <- rbindlist(lapply(names(models), function(name) {
    pred <- models[[name]]
    
    # Align prediction length with actual
    n <- length(pred)
    actual_trimmed <- tail(actual, n)
    
    # Evaluate
    metrics <- evaluate_model(actual_trimmed, pred)
    as.data.table(metrics)[, Model := name]
  }), fill = TRUE)
  
  setcolorder(evals, c("Model", "MAE", "RMSE", "MAPE", "R2"))
  print(evals)
  
  # Plotting
  pred_df <- data.table(Time = test_time, Actual = actual)
  
  for (name in names(models)) {
    pred <- models[[name]]
    n <- length(pred)
    
    # Initialize column with NA
    pred_df[[name]] <- NA
    
    # Fill only the last n rows
    pred_df[[name]][(nrow(pred_df) - n + 1):nrow(pred_df)] <- pred
  }
  
  pred_long <- melt(pred_df, id.vars = "Time", variable.name = "Model", value.name = "Value")
  
  ggplot(pred_long, aes(x = Time, y = Value, color = Model)) +
    geom_line() +
    labs(title = "Forecast vs Actual", y = "kWh", x = NULL) +
    theme_minimal()
}

# ================== Data Preparation ==================
prepare_data <- function(data, horizon) {
  data <- copy(data)
  data <- na.omit(data)
  
  # Shift target to only use lagged variables!
  data[, target := shift(total_consumption_kWh, -horizon)]
  
  data <- na.omit(data)
  return(data)
}
split_data <- function(data, split_ratio = 0.8) {
  n <- nrow(data)
  idx <- floor(n * split_ratio)
  list(train = data[1:idx], test = data[(idx + 1):n])
}