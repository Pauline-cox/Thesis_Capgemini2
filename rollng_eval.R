rolling_eval <- function(data, model_fit_func, model_predict_func, feature_matrix = FALSE) {
  data[, month := format(interval, "%Y-%m")]
  months <- unique(data$month)
  
  # Filter only Oct–Dec 2025
  eval_months <- months[months %in% c("2024-10", "2024-11", "2024-12")]
  results <- list()
  step_counter <- 1
  total_steps <- length(eval_months)
  
  for (m in eval_months) {
    cat(sprintf("Processing month %s (%d of %d)...\n", m, step_counter, total_steps))
    start_time <- Sys.time()
    
    train_months <- months[1:which(months == m) - 1]
    train_set <- data[month %in% train_months]
    test_set <- data[month == m]
    
    model <- model_fit_func(train_set)
    if (feature_matrix) {
      x_test <- model.matrix(get_model_formula(), test_set)[, -1]
      pred <- predict(model, x_test)
    } else {
      pred <- predict(model, test_set)
    }
    
    elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 2)
    cat(sprintf("Finished %s in %s seconds.\n\n", m, elapsed))
    
    results[[m]] <- data.table(
      interval = test_set$interval,
      actual = test_set$target,
      pred = pred
    )
    
    step_counter <- step_counter + 1
  }
  
  rbindlist(results)
}
rf_results <- rolling_eval(data, fit_rf, predict)
xgb_results <- rolling_eval(data, fit_xgb, predict, feature_matrix = TRUE)


library(data.table)
library(ggplot2)

# Combine results into list
models <- list(
  RandomForest = rf_results,
  XGBoost = xgb_results
)

# Plot forecast vs actual for each model
for (model_name in names(models)) {
  result <- models[[model_name]]
  
  plot_df <- data.table(
    Time = result$interval,
    Actual = result$actual,
    Prediction = result$pred
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

