# ==== 0. Input hourly data ====================================================
dt <- as.data.table(raw_data)
dt <- dt[!is.na(interval)]
setorder(dt, interval)

# ==== 1. Define CV folds (Aug, Sept, Oct 2024) ===============================
cv_months <- c("2024-08", "2024-09", "2024-10")

results_list <- list()
metrics_list <- list()
models_list  <- list()
residuals_list <- list()

for (cv_month in cv_months) {
  cat(sprintf("\n===== CV Fold: %s =====\n", cv_month))
  
  test_start <- ymd_hms(paste0(cv_month, "-01 00:00:00"), tz="UTC")
  test_end   <- floor_date(test_start, "month") + months(1) - hours(1)
  
  train_start <- as.POSIXct("2023-05-01 00:00:00", tz="UTC")
  train_end   <- test_start - hours(1)
  
  train_dt <- dt[interval >= train_start & interval <= train_end]
  test_dt  <- dt[interval >= test_start & interval <= test_end]
  
  if (nrow(test_dt) == 0) {
    cat(sprintf("Skipping %s — no data available.\n", cv_month))
    next
  }
  
  y_train <- ts(train_dt$total_consumption_kWh, frequency = 168)
  y_test  <- test_dt$total_consumption_kWh
  h       <- length(y_test)
  
  # ==== 2. Rolling forecast ==================================================
  all_forecasts <- numeric(0)
  all_actuals <- numeric(0)
  all_timestamps <- as.POSIXct(character(0))
  
  fit <- NULL
  for (i in seq(1, h, by = 24)) {
    current_end <- min(i + 23, h)
    current_test <- y_test[i:current_end]
    current_train <- ts(c(y_train, head(y_test, i - 1)), frequency = 168)
    
    if (is.null(fit)) {
      fit <- Arima(current_train,
                   order = c(1,0,1),
                   seasonal = list(order = c(1,1,1), period = 168),
                   include.mean = TRUE,
                   method = "CSS")
    } else {
      fit <- Arima(current_train,
                   order = c(1,0,1),
                   seasonal = list(order = c(1,1,1), period = 168),
                   include.mean = TRUE,
                   method = "CSS",
                   model = fit)
    }
    
    fc <- forecast(fit, h = length(current_test))
    all_forecasts <- c(all_forecasts, as.numeric(fc$mean))
    all_actuals <- c(all_actuals, current_test)
    all_timestamps <- c(all_timestamps, test_dt$interval[i:current_end])
  }
  
  res_dt <- data.table(interval = all_timestamps,
                       actual = all_actuals,
                       forecast = all_forecasts)
  
  # ==== 3. Metrics ===========================================================
  residuals <- res_dt$actual - res_dt$forecast
  MSE  <- mean(residuals^2, na.rm = TRUE)
  RMSE <- sqrt(MSE)
  MAE  <- mean(abs(residuals), na.rm = TRUE)
  
  valid_idx <- which(abs(res_dt$actual) > 1e-8)
  MAPE <- if (length(valid_idx) > 0) {
    mean(abs(residuals[valid_idx] / res_dt$actual[valid_idx])) * 100
  } else NA
  sMAPE <- mean(2*abs(residuals) / 
                  (abs(res_dt$actual) + abs(res_dt$forecast)), na.rm = TRUE) * 100
  
  metrics <- data.table(
    month = cv_month,
    MSE = MSE,
    RMSE = RMSE,
    MAE = MAE,
    MAPE = MAPE,
    sMAPE = sMAPE
  )
  
  results_list[[cv_month]] <- res_dt
  metrics_list[[cv_month]] <- metrics
  models_list[[cv_month]]  <- fit
  residuals_list[[cv_month]] <- residuals
  
  cat(sprintf("Fold %s: RMSE = %.2f, MAPE = %.2f%%, sMAPE = %.2f%%\n", 
              cv_month, RMSE, MAPE, sMAPE))
}

# ==== 4. Collect metrics & averages ==========================================
metrics_all <- rbindlist(metrics_list, fill = TRUE)
metrics_avg <- metrics_all[, lapply(.SD, mean, na.rm = TRUE), .SDcols = -"month"]
print(metrics_all)
cat("\nAverage metrics across folds:\n")
print(metrics_avg)

# ==== 5. Glue forecasts together for one plot ================================
res_all <- rbindlist(results_list, idcol = "month")

p_all <- ggplot(res_all, aes(x = interval)) +
  geom_line(aes(y = actual, colour = "Actual"), linewidth = 0.5) +
  geom_line(aes(y = forecast, colour = "Forecast"), linewidth = 0.5) +
  labs(title = "SARIMA Forecast vs Actuals (CV: Aug–Oct 2024)",
       x = "Time", y = "kWh") +
  scale_colour_manual(values = c("Actual" = "black", "Forecast" = "darkred")) +
  theme_minimal() +
  theme(legend.title = element_blank())
print(p_all)

# ==== 6. Print model fits ====================================================
for (cv_month in names(models_list)) {
  cat(sprintf("\n===== Model Coefficients for %s =====\n", cv_month))
  print(coef(models_list[[cv_month]]))
}

# ==== 7. Residual analysis per fold ==========================================
for (cv_month in names(models_list)) {
  cat(sprintf("\n===== Residual Analysis for %s =====\n", cv_month))
  checkresiduals(models_list[[cv_month]])
}