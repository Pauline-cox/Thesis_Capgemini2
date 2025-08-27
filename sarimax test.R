suppressPackageStartupMessages({
  library(data.table)
  library(forecast)
  library(ggplot2)
  library(lubridate)
})

# ==== 0. Input data (use smaller slice for testing) ==========================
dt <- as.data.table(model_data)
setorder(dt, interval)

# Only 3 months training + 2 weeks testing (instead of full year)
train_start <- as.POSIXct("2023-09-01 00:00:00", tz="UTC")
train_end   <- as.POSIXct("2024-11-30 23:00:00", tz="UTC")
test_start  <- as.POSIXct("2024-12-01 00:00:00", tz="UTC")
test_end    <- as.POSIXct("2024-12-31 23:00:00", tz="UTC")

train_dt <- dt[interval >= train_start & interval <= train_end]
test_dt  <- dt[interval >= test_start & interval <= test_end]

y_train <- ts(train_dt$total_consumption_kWh, frequency =168)   # daily seasonality for speed
y_test  <- test_dt$total_consumption_kWh
h       <- length(y_test)

# ==== 1. External regressors (reduced set) ==================================
# Instead of many lags, keep only key drivers + Fourier terms
xreg_vars <- c("total_occupancy", "tempC", "co2", "lux", "lag_24", "weekday")

X_train <- as.matrix(train_dt[, ..xreg_vars])
X_test  <- as.matrix(test_dt[, ..xreg_vars])

# Fourier terms to capture weekly seasonality without heavy AR terms
K <- 3  # harmonics, adjust for complexity
four_train <- fourier(ts(y_train, frequency = 168), K = K)
four_test  <- fourier(ts(c(y_train, y_test), frequency = 168), K = K, h = h)

X_train <- cbind(X_train, four_train)
X_test  <- cbind(X_test, four_test)

# ==== 2. Fit SARIMAX (fast settings) ========================================
fit <- auto.arima(
  y_train,
  xreg = X_train,
  seasonal = TRUE,
  stepwise = TRUE,        # fast search
  approximation = TRUE,   # use approximations for speed
  max.p = 2, max.q = 2, max.P = 1, max.Q = 1,
  allowdrift = TRUE, allowmean = TRUE
)

summary(fit)

# ==== 1. Define SARIMA(1,0,1)(1,1,1)[168] with regressors ================
fit <- Arima(
  y_train,
  order = c(1,0,1),          # (p,d,q)
  seasonal = list(order = c(1,1,1), period = 168),  # (P,D,Q)[s]
  xreg = X_train,
  include.mean = TRUE,
  method = "CSS"
)

summary(fit)

# ==== 3. Forecast ============================================================
fc <- forecast(fit, h = h, xreg = X_test)

res_dt <- data.table(interval = test_dt$interval,
                     actual = y_test,
                     forecast = as.numeric(fc$mean))

MSE  <- mean((res_dt$actual - res_dt$forecast)^2)
RMSE <- sqrt(MSE)
MAE  <- mean(abs(res_dt$actual - res_dt$forecast))
MAPE <- mean(abs((res_dt$actual - res_dt$forecast)/res_dt$actual), na.rm=TRUE)*100

cat("\n==== QUICK SARIMAX RESULTS ===\nMSE=%.2f | RMSE=%.2f | MAE=%.2f | MAPE=%.2f%%\n",
  MSE, RMSE, MAE, MAPE
))

# ==== 4. Plot ================================================================
ggplot(res_dt, aes(x = interval)) +
  geom_line(aes(y = actual, colour = "Actual"), linewidth = 0.4) +
  geom_line(aes(y = forecast, colour = "Forecast"), linewidth = 0.4) +
  labs(title = "Quick SARIMAX Forecast vs Actuals",
       subtitle = "3 months training, 2 weeks testing, Fourier + small regressor set",
       y = "kWh") +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "darkred")) +
  theme_minimal() +
  theme(legend.title = element_blank())
