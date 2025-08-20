library(data.table)
library(lubridate)
library(forecast)
library(ggplot2)

# ------------------------------------------------------------------
# Run SARIMA monthly-retrain for multiple horizons
# ------------------------------------------------------------------
run_sarima_for_horizons <- function(model_data,
                                    horizons = c(1, 24, 168),
                                    seasonal_period = 168,   # 24 (daily) or 168 (weekly)
                                    auto_args = list(stepwise = TRUE, approximation = FALSE)) {
  results <- list()
  metrics_list <- list()
  plots <- list()
  
  for (h in horizons) {
    cat(sprintf("\n============================= HORIZON = %d =============================\n", h))
    
    # Prepare target + split for this horizon
    data_h <- prepare_target(model_data, h)
    sets_h <- split_data(data_h)
    train_h <- sets_h$train
    test_h  <- sets_h$test
    actual_h <- test_h$target
    
    # Fit & predict with monthly re-train SARIMA
    t0 <- Sys.time()
    pred_h <- sarima_monthly_retrain(
      train_dt        = train_h,
      test_dt         = test_h,
      horizon         = h,
      seasonal_period = seasonal_period,
      auto_args       = auto_args
    )
    t1 <- Sys.time()
    runtime_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    
    # Metrics
    m <- eval_metrics(actual_h, pred_h)
    m$runtime_seconds <- runtime_sec
    m$horizon <- h
    metrics_list[[as.character(h)]] <- m
    
    # Plot (build plot_dt here)
    plot_dt <- data.table(
      target_time = test_h$interval + hours(h),
      actual      = actual_h,
      pred        = pred_h
    )
    gg <- ggplot(plot_dt, aes(x = target_time)) +
      geom_line(aes(y = actual, color = "Actual"), linewidth = 0.7) +
      geom_line(aes(y = pred,   color = "Predicted"), linewidth = 0.7, linetype = "dashed") +
      scale_color_manual(values = c("Actual" = "#1f77b4", "Predicted" = "#d62728")) +
      labs(
        title = sprintf("SARIMA (monthly retrain) — h = %d, season = %d", h, seasonal_period),
        x = "Time (target timestamp)",
        y = "Energy consumption (kWh)",
        color = "Series",
        caption = sprintf("MAE=%.2f | RMSE=%.2f | MAPE=%.2f%% | R²=%.3f | Run=%.1fs",
                          m$MAE, m$RMSE, m$MAPE, m$R2, runtime_sec)
      ) +
      theme_minimal()
    
    plots[[as.character(h)]] <- gg
    
    # Store artifacts
    results[[as.character(h)]] <- list(
      horizon = h,
      pred = pred_h,
      actual = actual_h,
      test_interval = test_h$interval,
      metrics = m,
      plot = gg
    )
    
    gc()
  }
  
  metrics_tbl <- rbindlist(metrics_list, use.names = TRUE, fill = TRUE)
  setorder(metrics_tbl, horizon)
  
  list(results = results, metrics = metrics_tbl, plots = plots)
}

# -------------------- RUN IT --------------------
# Assumes you already have:
# raw_data   <- load_and_prepare_data()
# model_data <- prepare_features(raw_data)
# sarima_monthly_retrain(), eval_metrics() defined as in your script

sarima_out <- run_sarima_for_horizons(
  model_data       = model_data,     # or model_data_small
  horizons         = c(1, 24, 168),
  seasonal_period  = 168,            # try 24 vs 168 and compare
  auto_args        = list(stepwise = TRUE, approximation = FALSE)
)

# Comparison table
print(sarima_out$metrics)

# Show plots (per horizon)
print(sarima_out$plots[["1"]])
print(sarima_out$plots[["24"]])
print(sarima_out$plots[["168"]])




# small helper: default operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------- monthly re-train SARIMA backtest (updates state inside month) ----------
sarima_monthly_retrain <- function(train_dt, test_dt, horizon,
                                   seasonal_period = 24,
                                   auto_args = list(stepwise = TRUE, approximation = FALSE)) {
  # bind and sort once
  all_dt <- rbindlist(list(copy(train_dt), copy(test_dt)), use.names = TRUE, fill = TRUE)
  setorder(all_dt, interval)
  
  # id months in test
  test_dt <- copy(test_dt)
  test_dt[, month_id := floor_date(interval, "month")]
  months_in_test <- unique(test_dt$month_id)
  
  # storage for predictions aligned with test rows
  preds <- rep(NA_real_, nrow(test_dt))
  
  # convenience accessor: series up to a given time
  get_series_upto <- function(t_until) {
    all_dt[interval <= t_until, total_consumption_kWh]
  }
  
  for (m in months_in_test) {
    idx_month_test <- which(test_dt$month_id == m)
    ts_month       <- test_dt$interval[idx_month_test]
    
    # train up to just before this month starts
    train_end_time <- min(ts_month) - seconds(1)
    y_train <- get_series_upto(train_end_time)
    
    # fit once for the month (univariate SARIMA)
    fit <- auto.arima(
      y = ts(y_train, frequency = seasonal_period),
      seasonal      = TRUE,
      stepwise      = auto_args$stepwise %||% TRUE,
      approximation = auto_args$approximation %||% FALSE
    )
    
    # within the month: rolling origin — update state (not parameters) and forecast h-ahead
    for (j in seq_along(idx_month_test)) {
      t_origin <- ts_month[j]
      y_upto   <- get_series_upto(t_origin)
      
      upd <- Arima(ts(y_upto, frequency = seasonal_period), model = fit)
      fc  <- forecast(upd, h = horizon)
      preds[idx_month_test[j]] <- as.numeric(fc$mean[horizon])
    }
  }
  
  # return in the same order as input test_dt
  preds
}

# ---------- evaluation (MAE, RMSE, MAPE, R2) ----------
eval_metrics <- function(actual, pred) {
  stopifnot(length(actual) == length(pred))
  ok <- is.finite(actual) & is.finite(pred)
  
  sse <- sum((pred[ok] - actual[ok])^2)
  sst <- sum((actual[ok] - mean(actual[ok]))^2)
  r2  <- if (sst > 0) (1 - sse / sst) else NA_real_
  
  ae   <- abs(pred[ok] - actual[ok])
  rmse <- sqrt(mean((pred[ok] - actual[ok])^2))
  mae  <- mean(ae)
  mape <- mean(ae / pmax(1e-8, abs(actual[ok]))) * 100
  
  data.table(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r2)
}
