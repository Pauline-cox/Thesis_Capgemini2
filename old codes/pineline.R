# ==================== Environment Setup ====================
source("set_up.R"); initialize_environment()
# source("load_data.R")
source("load_data_new.R")
source("features.R")
source("extra_functions.R")
source("models.R")

set.seed(1234)  # For R
tensorflow::set_random_seed(1234)  # For TensorFlow/Keras

# Load and prepare data
raw_data <- load_and_prepare_data()
model_data <- prepare_features(raw_data)

# Set forecast horizon
horizon <- 1

data <- prepare_data(model_data, horizon)  
sets <- split_data(data)
train_data <- sets$train
test_data <- sets$test
actual <- test_data$target

# # ==================== Model Formula ====================
# get_model_formula <- function() {
#   as.formula(
#     target ~ tempC + humidity + co2 + sound + lux + 
#       HH +
#       U + Q + FH + SQ + 
#       total_occupancy + office_hours + holiday + weekday + 
#       #hour_sin + 
#       #hour_cos +
#       lag_24 + lag_48 + lag_168 + lag_336 + lag_504 +
#       rollmean_24 + rollmean_168 
#   )
# }

get_model_formula <- function() {
  as.formula(
    target ~ co2 +
      total_occupancy + office_hours +
      hour_sin +
      lag_24 + lag_168 + lag_336 + lag_504  )
}

get_model_formula <- function() {
  as.formula(
    target ~ co2 + total_occupancy + office_hours + is_weekend +
      hour_cos +
      lag_24 + lag_48 + lag_168 + lag_336 + lag_504 +
      rollmean_24 + rollmean_168 +
      lux + sound + tempC + humidity +
      U + Q + FH + SQ
  )
}

get_model_formula <- function() {
  as.formula(
    target ~ co2 + total_occupancy + office_hours + is_weekend +
      hour_cos + hour_sin + 
      lag_24 + lag_48 + lag_72 + lag_168 + lag_336 + lag_504 +
      rollmean_24 + rollmean_168 + 
      lux + sound + tempC + humidity + 
      wind_speed + sunshine_minutes + global_radiation +
      humidity_percent + fog + rain + snow + thunder + ice + 
      date + hour +
      weekday + holiday 
  )
}



# ==================== Train & Predict ====================
x_test <- model.matrix(get_model_formula(), test_data)[, -1]

sarima_model <- fit_sarima(train_data)
pred_sarima <- forecast(sarima_model, h = nrow(test_data))$mean

sarimax_model <- fit_sarimax(train_data)
pred_sarimax <- forecast(sarimax_model, xreg = x_test)$mean

rf_model <- fit_rf(train_data)
print(rf_model)
plot(rf_model)
pred_rf <- predict(rf_model, test_data)

xgb_model <- fit_xgb(train_data)
print(xgb_model)
plot(xgb_model)
pred_xgb <- predict(xgb_model, x_test)

lstm_data <- prepare_lstm(data)
input_shape <- dim(lstm_data$x_train)[2:3]
lstm_model <- train_lstm(input_shape, lstm_data$x_train, lstm_data$y_train)
pred_lstm_normalized <- as.vector(predict(lstm_model, lstm_data$x_test))
pred_lstm <- pred_lstm_normalized * (lstm_data$target_max - lstm_data$target_min) + lstm_data$target_min

hybrid_model <- fit_sarimax_lstm_hybrid(train_data)
pred_hybrid <- predict_sarimax_lstm(hybrid_model, test_data)

# ==================== Evaluation ====================
models <- list(
  SARIMA = pred_sarima,
  SARIMAX = pred_sarimax,
  RandomForest = pred_rf,
  XGBoost = pred_xgb,
  LSTM = pred_lstm,
 Hybrid = pred_hybrid
)
models <- list(
  SARIMA = pred_sarima,
  RandomForest = pred_rf,
  XGBoost = pred_xgb)

evaluate_models(models, actual, test_data$interval, lstm_data)

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

