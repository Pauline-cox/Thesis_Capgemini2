# ==================== Environment Setup ====================
source("set_up.R"); initialize_environment()
source("load_data_new.R")
source("features.R")
source("extra_functions.R")
source("models.R")

library(ggplot2)
library(data.table)

set.seed(1234)
tensorflow::set_random_seed(1234)

# Define horizons to evaluate
horizons <- c(1, 4, 24, 168, 672)  # in hours

# Create storage lists
all_evaluations <- list()

# Load and prepare data once
raw_data <- load_and_prepare_data()
model_data <- prepare_features(raw_data)

# Loop through forecast horizons
for (h in horizons) {
  cat("----- Running models for HORIZON =", h, "-----\n")
  
  # Prepare data
  data <- prepare_data(model_data, h)
  sets <- split_data(data)
  train_data <- sets$train
  test_data <- sets$test
  actual <- test_data$target
  
  # Formula
  get_model_formula <- function() {
    as.formula(
      target ~ co2 +
        total_occupancy + office_hours + 
        hour_sin +
        lag_24 + lag_168 + lag_336 + lag_504
    )
  }
  
  # Fit and predict
  cat("Training SARIMA...\n")
  sarima_model <- fit_sarima(train_data)
  pred_sarima <- forecast(sarima_model, h = nrow(test_data))$mean
  
  cat("Training SARIMAX...\n")
  sarimax_model <- fit_sarimax(train_data)
  x_test <- model.matrix(get_model_formula(), test_data)[, -1]
  pred_sarimax <- forecast(sarimax_model, xreg = x_test)$mean
  
  cat("Training Random Forest...\n")
  rf_model <- fit_rf(train_data)
  pred_rf <- predict(rf_model, test_data)
  
  cat("Training XGBoost...\n")
  xgb_model <- fit_xgb(train_data)
  pred_xgb <- predict(xgb_model, x_test)
  
  cat("Training LSTM...\n")
  lstm_data <- prepare_lstm(data)
  input_shape <- dim(lstm_data$x_train)[2:3]
  lstm_model <- train_lstm(input_shape, lstm_data$x_train, lstm_data$y_train)
  pred_lstm_normalized <- as.vector(predict(lstm_model, lstm_data$x_test))
  pred_lstm <- pred_lstm_normalized * (lstm_data$target_max - lstm_data$target_min) + lstm_data$target_min
  
  cat("Training Hybrid Model...\n")
  hybrid_model <- fit_sarimax_lstm_hybrid(train_data)
  pred_hybrid <- predict_sarimax_lstm(hybrid_model, test_data)
  
  # Evaluation
  models <- list(
    SARIMA = pred_sarima,
    SARIMAX = pred_sarimax,
    RandomForest = pred_rf,
    XGBoost = pred_xgb,
    LSTM = pred_lstm,
    Hybrid = pred_hybrid
  )
  
  eval_results <- evaluate_models(models, actual, test_data$interval, lstm_data)
  eval_results[, horizon := h]
  all_evaluations[[as.character(h)]] <- eval_results
  
  # Save plots
  plot_dir <- "plots/horizon_evaluation"
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (model_name in names(models)) {
    pred <- models[[model_name]]
    actual_vec <- if (model_name == "LSTM") lstm_data$y_test_original else actual
    n <- min(length(pred), length(actual_vec))
    
    plot_df <- data.table(
      Time = tail(test_data$interval, n),
      Actual = tail(actual_vec, n),
      Prediction = tail(pred, n)
    )
    
    p <- ggplot(plot_df, aes(x = Time)) +
      geom_line(aes(y = Actual), color = "black", size = 1) +
      geom_line(aes(y = Prediction), color = "blue", size = 1, linetype = "dashed") +
      labs(
        title = paste(model_name, "- Forecast vs Actual (Horizon:", h, ")"),
        x = "Time",
        y = "Energy Consumption (kWh)"
      ) +
      theme_minimal()
    
    ggsave(filename = file.path(plot_dir, paste0(model_name, "_h", h, ".png")),
           plot = p, width = 8, height = 4)
  }
}

# Combine and save evaluation results
setDT(eval_results)

eval_results[, horizon := h]
fwrite(eval_df, "model_evaluation_by_horizon.csv")

cat("✅ All horizons evaluated and results saved.\n")
