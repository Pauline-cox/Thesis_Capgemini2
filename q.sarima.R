# --- libs ---
library(data.table)
library(lubridate)
library(forecast)
library(ggplot2)


# ---------- CONFIG ----------


# Your existing pipeline (as given)
raw_data   <- load_and_prepare_data()
model_data <- prepare_features(raw_data)
model_data_small <- head(model_data, n = nrow(model_data) / 1)

horizon <- 1
data <- prepare_target(model_data_small, horizon)
sets <- split_data(data)
train_data <- sets$train
test_data  <- sets$test
actual     <- test_data$target

# choose seasonality for hourly data (try 24 or 24*7)
seasonal_period <- 168

# small helper: default operator
'%||%' <- function(a, b) if (!is.null(a)) a else b


# --- sanity check for your schema ---
stopifnot(all(c("interval", "total_consumption_kWh") %in% names(train_data)))


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
      seasonal     = TRUE,
      stepwise     = auto_args$stepwise %||% TRUE,
      approximation= auto_args$approximation %||% FALSE
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

# ---------- run the backtest with runtime tracking ----------
t_start <- Sys.time()
pred <- sarima_monthly_retrain(
  train_dt        = train_data,
  test_dt         = test_data,
  horizon         = horizon,
  seasonal_period = seasonal_period
)
t_end   <- Sys.time()
runtime_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))
# cat(sprintf("Monthly SARIMA backtest runtime: %.2f seconds\n", runtime_sec))


# ---------- evaluation (MAE, RMSE, MAPE, R2) ----------
eval_metrics <- function(actual, pred) {
  stopifnot(length(actual) == length(pred))
  ok <- is.finite(actual) & is.finite(pred)

  sse <- sum((pred[ok] - actual[ok])^2)
  sst <- sum((actual[ok] - mean(actual[ok]))^2)
  r2  <- if (sst > 0) (1 - sse/sst) else NA_real_

  ae   <- abs(pred[ok] - actual[ok])
  rmse <- sqrt(mean((pred[ok] - actual[ok])^2))
  mae  <- mean(ae)
  mape <- mean(ae / pmax(1e-8, abs(actual[ok]))) * 100

  data.table(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r2)
}

metrics <- eval_metrics(actual, pred)
metrics[, runtime_seconds := runtime_sec]
print(metrics)

# ---------- plot: Pred vs Actual at the target timestamps (colored) ----------
# plot_dt <- data.table(
#   interval_origin = test_data$interval,
#   target_time     = test_data$interval + lubridate::hours(horizon),
#   actual          = actual,
#   pred            = pred
# )

gg <- ggplot(plot_dt, aes(x = target_time)) +
  geom_line(aes(y = actual,   color = "Actual"),   linewidth = 0.7) +
  geom_line(aes(y = pred,     color = "Predicted"), linewidth = 0.7, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "#1f77b4", "Predicted" = "#d62728")) +
  labs(
    title = sprintf("SARIMA (monthly retrain) — horizon = %d, season = %d", horizon, seasonal_period),
    x = "Time (target timestamp)",
    y = "Energy consumption (kWh)",
    color = "Series"  ) +
  theme_minimal()

print(gg)
