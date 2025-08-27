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
test_end    <- as.POSIXct("2024-12-31 23:00:00", tz="UTC")

train_dt <- dt[interval >= train_start & interval <= train_end]
test_dt  <- dt[interval >= test_start & interval <= test_end]

y_train <- ts(train_dt$total_consumption_kWh, frequency = 168)
y_test  <- test_dt$total_consumption_kWh
h       <- length(y_test)

# Choose regressors
xreg_vars <-   c("total_occupancy","co2","lux","lag_24","global_radiation")
X_train <- as.matrix(train_dt[, ..xreg_vars])
X_test  <- as.matrix(test_dt[, ..xreg_vars])

# ==== 2. ROLLING FORECAST SETUP ==============================================
rolling_steps <- 24
all_forecasts <- all_actuals <- all_residuals <- numeric(0)
all_timestamps <- as.POSIXct(character(0))
model_diagnostics <- list()

# ==== 3. ROLLING FORECAST LOOP ===============================================
start_time <- Sys.time()

for (i in seq(1, h, by = rolling_steps)) {
  current_end <- min(i + rolling_steps - 1, h)
  cat(sprintf("Forecasting hours %d to %d of %d...\n", i, current_end, h))
  
  # Slice
  current_test <- y_test[i:current_end]
  current_train <- ts(c(y_train, y_test[0:(i-1)]), frequency = 168)
  X_train_ext <- rbind(X_train, X_test[0:(i-1), , drop=FALSE])
  X_test_ext  <- X_test[i:current_end, , drop=FALSE]
  
  # Fit SARIMAX
  if (i == 1) {
    fit <- Arima(
      current_train,
      order = c(1,0,1),
      seasonal = list(order = c(1,1,1), period = 168),
      xreg = X_train_ext,
      include.mean = TRUE,
      method = "CSS"
    )
  } else {
    fit <- Arima(
      current_train,
      order = c(1,0,1),
      seasonal = list(order = c(1,1,1), period = 168),
      xreg = X_train_ext,
      include.mean = TRUE,
      method = "CSS",
      model = fit
    )
  }
  
  # Forecast
  fc <- forecast(fit, h = length(current_test), xreg = X_test_ext)
  current_forecast <- as.numeric(fc$mean)
  current_residuals <- current_test - current_forecast
  
  # Store
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
  
  # Timing info
  elapsed <- as.numeric(Sys.time() - start_time, units = "mins")
  est_remaining <- (elapsed / i) * (h - i) / 60  # in hours
  cat(sprintf("Elapsed: %.1f minutes | Est. remaining: %.1f hours\n",
              elapsed, est_remaining))
}

# ==== 4. RESULTS =============================================================
res_dt <- data.table(interval = all_timestamps,
                     actual = all_actuals,
                     forecast = all_forecasts,
                     residual = all_residuals)

MSE  <- mean(all_residuals^2)
RMSE <- sqrt(MSE)
MAE  <- mean(abs(all_residuals))
MAPE <- mean(abs(all_residuals / all_actuals), na.rm=TRUE) * 100

cat(sprintf(
  "\n==== ROLLING SARIMAX RESULTS (xreg = occupancy, co2, lag_24) ====\nMSE = %.2f | RMSE = %.2f | MAE = %.2f | MAPE = %.2f%% | Total time: %.1f minutes\n",
  MSE, RMSE, MAE, MAPE, as.numeric(Sys.time() - start_time, units = "mins")
))

# ==== 5. PLOT ================================================================
ggplot(res_dt, aes(x = interval)) +
  geom_line(aes(y = actual, colour = "Actual"), linewidth = 0.4) +
  geom_line(aes(y = forecast, colour = "Forecast"), linetype = "dashed", linewidth = 0.4) +
  labs(title = "Rolling SARIMAX Forecast vs Actuals",
       subtitle = "Window = expanding, Step = 24h",
       x = "Time", y = "Consumption (kWh)") +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "blue")) +
  theme_minimal() +
  theme(legend.title = element_blank())
# ==== 5. COMPREHENSIVE RESIDUAL ANALYSIS =====================================
cat("\n==== RESIDUAL ANALYSIS ====\n")

# 5.1 Basic Statistics
cat("\nResidual Statistics:\n")
cat(sprintf("Mean: %.4f (should be close to 0)\n", mean(all_residuals)))
cat(sprintf("Std Dev: %.4f\n", sd(all_residuals)))
cat(sprintf("Skewness: %.4f (should be close to 0)\n", moments::skewness(all_residuals)))
cat(sprintf("Kurtosis: %.4f (should be close to 3)\n", moments::kurtosis(all_residuals)))

# 5.2 Normality Tests
cat("\nNormality Tests:\n")
tryCatch({
  if(length(all_residuals) <= 5000) {
    shapiro <- shapiro.test(all_residuals)
    cat(sprintf("Shapiro-Wilk: p-value = %.4f\n", shapiro$p.value))
  }
}, error = function(e) cat("Shapiro-Wilk: Sample too large\n"))

jb_test <- jarque.bera.test(all_residuals)
cat(sprintf("Jarque-Bera: p-value = %.4f\n", jb_test$p.value))

# 5.3 Autocorrelation Tests
cat("\nAutocorrelation Tests:\n")
lb_test_24 <- Box.test(all_residuals, lag = 24, type = "Ljung-Box")
lb_test_168 <- Box.test(all_residuals, lag = 168, type = "Ljung-Box")
cat(sprintf("Ljung-Box (24 lags): p-value = %.4f\n", lb_test_24$p.value))
cat(sprintf("Ljung-Box (168 lags): p-value = %.4f\n", lb_test_168$p.value))

# 5.4 Stationarity Test
cat("\nStationarity Test:\n")
adf_test <- adf.test(all_residuals)
cat(sprintf("ADF Test: p-value = %.4f\n", adf_test$p.value))

# ==== 6. RESIDUAL VISUALIZATION ==============================================
checkresiduals(fit)

# 6.1 Residual Time Series Plot
p1 <- ggplot(res_dt, aes(x = interval, y = residual)) +
  geom_line(color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time", x = "Time", y = "Residual (Actual - Forecast)") +
  theme_minimal()

# 6.2 Residual Distribution
p2 <- ggplot(res_dt, aes(x = residual)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(all_residuals), sd = sd(all_residuals)), 
                color = "green", linetype = "dashed") +
  labs(title = "Residual Distribution", x = "Residual", y = "Density") +
  theme_minimal()


# 6.3 Q-Q Plot
p3 <- ggplot(res_dt, aes(sample = residual)) +
  stat_qq(color = "steelblue") +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# 6.4 ACF of Residuals
acf_data <- acf(all_residuals, plot = FALSE)
acf_df <- data.table(lag = acf_data$lag, acf = acf_data$acf)

p4 <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = "steelblue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-1, 1) * 1.96/sqrt(length(all_residuals)), 
             linetype = "dashed", color = "red") +
  labs(title = "ACF of Residuals", x = "Lag", y = "ACF") +
  theme_minimal()

# 6.5 Residuals vs Fitted
p5 <- ggplot(res_dt, aes(x = forecast, y = residual)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "darkgreen") +
  labs(title = "Residuals vs Forecasted Values", x = "Forecast", y = "Residual") +
  theme_minimal()

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)