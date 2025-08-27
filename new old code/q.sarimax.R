library(data.table)
library(lubridate)
library(forecast)


# ---------- CONFIG ----------


# Your existing pipeline (as given)
raw_data   <- load_and_prepare_data()
model_data <- prepare_features(raw_data)
model_data_small <- head(model_data, n = nrow(model_data) / 1)

horizon <- 168*4
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


# --- robust NA-safe xreg builder ---
.make_xreg <- function(dt, xreg_cols, weekday_levels = NULL, all_dummy_names = NULL) {
  dt_loc <- copy(dt)
  
  # split requested columns
  cont_cols <- setdiff(xreg_cols, "weekday")
  
  # 1) Fill NAs in continuous columns (locf then nocb to cover edges)
  if (length(cont_cols)) {
    for (cc in cont_cols) {
      if (!cc %in% names(dt_loc)) stop(sprintf("xreg col '%s' not found", cc))
      v <- dt_loc[[cc]]
      # if not numeric, coerce
      if (!is.numeric(v)) v <- as.numeric(v)
      v <- data.table::nafill(v, type = "locf")
      v <- data.table::nafill(v, type = "nocb")
      # any remaining NAs -> 0
      v[!is.finite(v)] <- 0
      dt_loc[[cc]] <- v
    }
  }
  
  # 2) Build continuous matrix
  X_cont <- NULL
  if (length(cont_cols)) {
    X_cont <- as.matrix(dt_loc[, ..cont_cols])
    storage.mode(X_cont) <- "numeric"
  }
  
  # 3) Weekday dummies (optional)
  X_w <- NULL
  if ("weekday" %in% xreg_cols) {
    # lock global levels; replace NA with first level to avoid drops
    if (is.null(weekday_levels)) {
      dt_loc[, weekday := factor(weekday)]
      weekday_levels <- levels(dt_loc$weekday)
    } else {
      dt_loc[, weekday := factor(weekday, levels = weekday_levels)]
    }
    # any NA -> first level (arbitrary but keeps row count)
    if (any(is.na(dt_loc$weekday))) {
      dt_loc$weekday[is.na(dt_loc$weekday)] <- weekday_levels[1]
    }
    # build dummies; keep rows (na.pass)
    X_w <- model.matrix(~ weekday - 1, data = dt_loc, na.action = na.pass)
    # replace NA dummy cells with 0
    X_w[!is.finite(X_w)] <- 0
    
    # ensure consistent dummy set/order
    if (is.null(all_dummy_names)) {
      all_dummy_names <- colnames(X_w)
    } else {
      missing <- setdiff(all_dummy_names, colnames(X_w))
      if (length(missing)) {
        X_w <- cbind(X_w,
                     matrix(0, nrow = nrow(X_w), ncol = length(missing),
                            dimnames = list(NULL, missing)))
      }
      X_w <- X_w[, all_dummy_names, drop = FALSE]
    }
  }
  
  # 4) Bind (rows MUST match)
  if (is.null(X_cont) && is.null(X_w)) stop("No usable xreg columns after preprocessing.")
  X <- if (is.null(X_cont)) X_w else if (is.null(X_w)) X_cont else cbind(X_cont, X_w)
  
  storage.mode(X) <- "numeric"
  list(X = X, weekday_levels = weekday_levels, all_dummy_names = all_dummy_names)
}

# --- Monthly re-train SARIMAX (state updates within the month) ---
sarimax_monthly_retrain <- function(train_dt, test_dt, horizon,
                                    xreg_cols,
                                    seasonal_period = 24,
                                    auto_args = list(stepwise = TRUE, approximation = FALSE)) {
  all_dt <- rbindlist(list(copy(train_dt), copy(test_dt)), use.names = TRUE, fill = TRUE)
  setorder(all_dt, interval)
  
  # consistent weekday levels across the whole run
  weekday_levels <- if ("weekday" %in% xreg_cols) levels(as.factor(all_dt$weekday)) else NULL
  
  test_dt <- copy(test_dt)
  test_dt[, month_id := floor_date(interval, "month")]
  months_in_test <- unique(test_dt$month_id)
  
  preds <- rep(NA_real_, nrow(test_dt))
  
  get_rows_upto    <- function(t_until) all_dt[interval <= t_until]
  get_rows_future_h<- function(t_origin, h) all_dt[interval > t_origin][1:h]
  
  all_dummy_names <- NULL
  
  for (m in months_in_test) {
    idx_month_test <- which(test_dt$month_id == m)
    ts_month       <- test_dt$interval[idx_month_test]
    
    train_end_time <- min(ts_month) - seconds(1)
    train_rows     <- get_rows_upto(train_end_time)
    
    y_train <- train_rows$total_consumption_kWh
    
    xr_train <- .make_xreg(train_rows, xreg_cols,
                           weekday_levels = weekday_levels,
                           all_dummy_names = all_dummy_names)
    X_train <- xr_train$X
    weekday_levels  <- xr_train$weekday_levels
    all_dummy_names <- xr_train$all_dummy_names
    
    fit <- auto.arima(
      y = ts(y_train, frequency = seasonal_period),
      xreg = X_train,
      seasonal = TRUE,
      stepwise = auto_args$stepwise %||% TRUE,
      approximation = auto_args$approximation %||% FALSE
    )
    
    for (j in seq_along(idx_month_test)) {
      t_origin <- ts_month[j]
      
      upto_rows <- get_rows_upto(t_origin)
      y_upto    <- upto_rows$total_consumption_kWh
      xr_upto   <- .make_xreg(upto_rows, xreg_cols,
                              weekday_levels = weekday_levels,
                              all_dummy_names = all_dummy_names)
      X_upto <- xr_upto$X
      
      upd <- Arima(ts(y_upto, frequency = seasonal_period),
                   xreg = X_upto,
                   model = fit)
      
      fut_rows <- get_rows_future_h(t_origin, horizon)
      if (nrow(fut_rows) < horizon) { preds[idx_month_test[j]] <- NA_real_; next }
      xr_fut <- .make_xreg(fut_rows, xreg_cols,
                           weekday_levels = weekday_levels,
                           all_dummy_names = all_dummy_names)
      X_fut <- xr_fut$X[1:horizon, , drop = FALSE]
      
      fc <- forecast(upd, h = horizon, xreg = X_fut)
      preds[idx_month_test[j]] <- as.numeric(fc$mean[horizon])
    }
  }
  
  preds
}


xreg_cols <- c("total_occupancy","tempC","humidity","co2","sound","lux","weekday", "lag_24", "lag_168")
xreg_cols <- c("total_occupancy","weekday","co2", "lag_24", "lag_168","rollmean_24", "hour_sin", "tempC", "office_hours")


t_start <- Sys.time()
pred <- sarimax_monthly_retrain(
  train_dt        = train_data,
  test_dt         = test_data,
  horizon         = horizon,        # 1 / 24 / 24*30 ...
  xreg_cols       = xreg_cols,
  seasonal_period = 24              # also try 24*7
)
t_end <- Sys.time()
runtime_sec <- as.numeric(difftime(t_end, t_start, units = "secs"))
cat(sprintf("Monthly SARIMAX runtime: %.2f sec\n", runtime_sec))


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
# safest
metrics$runtime_seconds <- runtime_sec
print(metrics)


# ---------- plot: Pred vs Actual at the target timestamps (colored) ----------
plot_dt <- data.table(
  interval_origin = test_data$interval,
  target_time     = test_data$interval + lubridate::hours(horizon),
  actual          = actual,
  pred            = pred
)

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

