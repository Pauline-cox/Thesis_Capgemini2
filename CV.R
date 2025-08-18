expanding_window_cv <- function(data, initial_window, horizon = 1, step = 1, model_function, verbose = TRUE, ...) {
  n <- nrow(data)
  forecasts <- c()
  actuals <- c()
  total_iters <- length(seq(initial_window, n - horizon, by = step))
  iter <- 0
  
  for (start in seq(initial_window, n - horizon, by = step)) {
    iter <- iter + 1
    if (verbose) {
      cat(sprintf("Iteration %d of %d | Training: 1:%d | Testing: %d:%d\n",
                  iter, total_iters, start, start + 1, start + horizon))
    }
    
    train <- data[1:start]
    test <- data[(start + 1):(start + horizon)]
    
    # Train the model
    model <- model_function(train, ...)
    
    # Predict depending on method
    pred <- predict(model, test)
    
    forecasts <- c(forecasts, as.vector(pred))
    actuals <- c(actuals, test$target)
  }
  
  list(pred = forecasts, actual = actuals)
}
cv_result_rf <- expanding_window_cv(
  data = data,
  initial_window = 5000,
  horizon = 1,
  step = 24,
  model_function = fit_rf,
  verbose = TRUE
)
evaluate_models(list(RF_CV = cv_result_rf$pred), cv_result_rf$actual, tail(data$interval, length(cv_result_rf$actual)))
