suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
  library(ggplot2)
  library(lubridate)
  library(tseries)
  library(moments)
})

# ==== 0. Input hourly data ====================================================
dt <- as.data.table(model_data)
setorder(dt, interval)

# ==== 1. Train/Test split ====================================================
train_start <- as.POSIXct("2023-09-01 00:00:00", tz="UTC")
train_end   <- as.POSIXct("2024-09-30 23:00:00", tz="UTC")
test_start  <- as.POSIXct("2024-10-01 00:00:00", tz="UTC")
test_end    <- as.POSIXct("2024-10-15 23:00:00", tz="UTC")

train_dt <- dt[interval >= train_start & interval <= train_end]
test_dt  <- dt[interval >= test_start & interval <= test_end]

y_train <- ts(train_dt$total_consumption_kWh, frequency = 168)
y_test  <- test_dt$total_consumption_kWh
h       <- length(y_test)

# External regressors
xreg_vars <- c("total_occupancy", "co2", "lux", "hour", "office_hours",
               "is_weekend", "lag_24", "lag_504", "rollsd_24")
X_train <- as.matrix(train_dt[, ..xreg_vars])
X_test  <- as.matrix(test_dt[, ..xreg_vars])

# ==== 2. ROLLING FORECAST SETUP ==============================================
window_size <- length(y_train)
rolling_steps <- 24

# Pre-allocate results
all_forecasts <- numeric(0)
all_actuals <- numeric(0)
all_timestamps <- as.POSIXct(character(0))
all_residuals <- numeric(0)
model_diagnostics <- list()

# ==== 3. OPTIMIZED ROLLING FORECAST ==========================================
start_time <- Sys.time()

for (i in seq(1, h, by = rolling_steps)) {
  current_end <- i + rolling_steps - 1
  if (current_end > h) current_end <- h
  
  cat(sprintf("Forecasting hours %d to %d of %d...\n", i, current_end, h))
  
  # Current slices
  current_test <- y_test[i:current_end]
  X_train_ext <- rbind(X_train, X_test[0:(i-1), , drop=FALSE])
  X_test_ext  <- X_test[i:current_end, , drop=FALSE]
  current_train <- ts(c(y_train, y_test[0:(i-1)]), frequency = 168)
  
  # Fit/update SARIMAX
  # if (i == 1) {
    fit <- auto.arima(
      current_train,
      xreg = X_train_ext,
      seasonal = TRUE,
      stepwise = TRUE,
      approximation = FALSE,
      max.p = 3, max.q = 3, max.P = 2, max.Q = 2, max.order = 5,
      allowmean = TRUE, allowdrift = TRUE,
      method = "CSS"
    )

  # } else {
    # fit <- auto.arima(
      # current_train,
      # xreg = X_train_ext,
      # seasonal = TRUE,
      # stepwise = TRUE,
      # approximation = TRUE,
      # max.p = 3, max.q = 3, max.P = 2, max.Q = 2, max.order = 5,
      # allowmean = TRUE, allowdrift = TRUE,
      # method = "CSS",
      # model = fit)
  # }
  
  # Forecast with external regressors
  fc <- forecast(fit, h = length(current_test), xreg = X_test_ext)
  current_forecast <- as.numeric(fc$mean)
  current_residuals <- current_test - current_forecast
  
  # Store results
  all_forecasts <- c(all_forecasts, current_forecast)
  all_actuals <- c(all_actuals, current_test)
  all_timestamps <- c(all_timestamps, test_dt$interval[i:current_end])
  all_residuals <- c(all_residuals, current_residuals)
  
  # Diagnostics every 168 hours
  if (i %% 168 == 1) {
    model_diagnostics[[as.character(i)]] <- list(
      iteration = i,
      aic = AIC(fit),
      bic = BIC(fit),
      log_likelihood = logLik(fit),
      coefficients = coef(fit)
    )
  }
}

# ==== 4. RESULTS & METRICS ===================================================
res_dt <- data.table(interval = all_timestamps,
                     actual = all_actuals,
                     forecast = all_forecasts,
                     residual = all_residuals)

MSE  <- mean((all_residuals)^2)
RMSE <- sqrt(MSE)
MAE  <- mean(abs(all_residuals))
MAPE <- mean(abs(all_residuals / all_actuals), na.rm=TRUE) * 100

cat("\n==== ROLLING SARIMAX FORECAST RESULTS ====\n")
cat(sprintf("MSE   = %.2f\n", MSE))
cat(sprintf("RMSE  = %.2f\n", RMSE))
cat(sprintf("MAE   = %.2f\n", MAE))
cat(sprintf("MAPE  = %.2f%%\n", MAPE))
cat(sprintf("Total time: %.1f minutes\n", as.numeric(Sys.time() - start_time, units = "mins")))

# ==== 5. Plot ================================================================
ggplot(res_dt, aes(x = interval)) +
  geom_line(aes(y = actual, colour = "Actual"), linewidth = 0.4) +
  geom_line(aes(y = forecast, colour = "Forecast"), linewidth = 0.4) +
  labs(title = "Rolling SARIMAX Forecast vs Actuals",
       subtitle = sprintf("Window: %d hours, Steps: %d hours", window_size, rolling_steps),
       x = "Time", y = "kWh") +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "darkred")) +
  theme_minimal() +
  theme(legend.title = element_blank())

