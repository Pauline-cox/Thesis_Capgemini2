# R/50_run.R
# -----------------------------------------------------------------------------
# Orchestrates the full run: load, prep, select variables, train models, evaluate.
# -----------------------------------------------------------------------------

source("n.init.R")
source("n.utils.R")
source("n.data.R")
# Your own data loaders/feature builders:
source("load_data_new.R")  # load_and_prepare_data()
source("features.R")       # prepare_features()
source("n.variable.selection.R")
source("n.models.R")

# --- Load & features ---
raw_data   <- load_and_prepare_data()
model_data <- prepare_features(raw_data)

# --- Forecast horizon ---
horizon <- 1
data <- prepare_data(model_data, horizon)
sets <- split_data(data)
train_data <- sets$train
test_data  <- sets$test
actual     <- test_data$target

# --- One-time variable selection on TRAIN only ---
sel <- select_variables_once(
  train_dt = as.data.table(train_data),
  target   = "target",
  features = candidate_features  # <- explicit to avoid ambiguity
)


cat("\n== Variable decisions ==\n")
print(sel$table[, .(variable, correlation, rf_rank, shap_rank, votes, decision)])
cat("\nSARIMAX features:\n"); print(sel$features_for_SARIMAX)
cat("\nTree/LSTM features:\n"); print(sel$features_for_TREES)

# --- Train & predict ---
log_step("Fit SARIMA",  { sarima_model  <- fit_sarima(train_data) })
pred_sarima <- forecast(sarima_model, h = nrow(test_data))$mean
  
log_step("Fit SARIMAX", { sarimax_model <- fit_sarimax(train_data, sel) })
x_test_sarimax <- model.matrix(get_formula_for_model("SARIMAX", sel), test_data)[, -1]
pred_sarimax <- forecast(sarimax_model, xreg = x_test_sarimax)$mean

log_step("Fit RandomForest", { rf_model  <- fit_rf(train_data, sel) })
pred_rf <- predict(rf_model, test_data)

log_step("Fit XGBoost",      { xgb_model <- fit_xgb(train_data, sel) })
x_test_xgb <- model.matrix(get_formula_for_model("XGBoost", sel), test_data)[, -1]
pred_xgb <- predict(xgb_model, x_test_xgb)

log_step("Fit LSTM", {
  lstm_data  <- prepare_lstm(train_data, sel)  # robust numeric-only pipeline
  lstm_model <- train_lstm(dim(lstm_data$x_train)[2:3], lstm_data$x_train, lstm_data$y_train)
})
pred_lstm <- as.vector(predict(lstm_model, lstm_data$x_test))
pred_lstm <- pred_lstm * (lstm_data$target_max - lstm_data$target_min) + lstm_data$target_min

log_step("Fit Hybrid SARIMA+LSTM", {
  hybrid_sarima <- fit_hybrid_sarima_lstm(train_data, sel, timesteps = 168)
})
pred_hybrid_sarima <- predict_hybrid_sarima_lstm(hybrid_sarima, train_data, test_data, sel)

log_step("Fit Hybrid SARIMAX+LSTM", {
  hybrid_sarimax <- fit_hybrid_sarimax_lstm(train_data, sel, timesteps = 168)
})
pred_hybrid_sarimax <- predict_hybrid_sarimax_lstm(hybrid_sarimax, train_data, test_data, sel)

# --- Evaluate ---
models <- list(
  SARIMA       = pred_sarima,
  SARIMAX      = pred_sarimax,
  RandomForest = pred_rf,
  XGBoost      = pred_xgb,
  LSTM         = pred_lstm,
  Hybrid_sarima       = pred_hybrid_sarima,
  Hybrid_sarimax       = pred_hybrid_sarimax
)

# Crucial: LSTM must be evaluated against the target that aligns with its sequences
actual_overrides <- list(
  LSTM = lstm_data$y_test_original
)

evals <- evaluate_models(
  models = models,
  default_actual = test_data$target,
  time_index = test_data$interval,
  actual_overrides = actual_overrides
)
plot_metrics_bars(evals)            # draws MAE/RMSE/MAPE/R2 bars
# or pick specific ones:
# plot_metrics_bars(evals, which = c("RMSE","R2"))


plot_aligned_forecasts(
  models,
  default_actual = test_data$target,
  time_index     = test_data$interval,
  actual_overrides = actual_overrides,
  facet = TRUE   # set FALSE if you prefer one combined panel with colors
)

# === Plot Each Forecast ===
for (model_name in names(models)) {
  pred <- models[[model_name]]
  
  # Use original scale for LaSTM
  actual_vec <- if (model_name == "LSTM") {
    lstm_data$y_test_original
  } else {
    actual
  }
  
  # Align lengths
  n <- min(length(pred), length(actual_vec))
  time_vals <- tail(test_data$interval, n)
  
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

residual_diagnostics(
  models,
  default_actual = test_data$target,
  time_index     = test_data$interval,
  actual_overrides = actual_overrides
)
