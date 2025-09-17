# ============================
# Building Load Forecast Bench
# ============================

suppressPackageStartupMessages({
  library(data.table); library(forecast); library(ggplot2)
  library(lubridate);   library(tseries);  library(moments)
  library(keras);       library(recipes)
})

# ---------- Global config ----------
SEASONAL_PERIOD <- 168L
ROLLING_STEPS   <- 24L
LOOKBACK        <- 96L
HORIZON         <- 24L

# Folds: test = Oct/Nov/Dec 2024; train = all data < fold start
FOLDS <- list(
  OCT = list(test_start = as.POSIXct("2024-10-01 00:00:00", tz="UTC"),
             test_end   = as.POSIXct("2024-10-31 23:00:00", tz="UTC")),
  NOV = list(test_start = as.POSIXct("2024-11-01 00:00:00", tz="UTC"),
           test_end   = as.POSIXct("2024-11-30 23:00:00", tz="UTC")),
  DEC = list(test_start = as.POSIXct("2024-12-01 00:00:00", tz="UTC"),
             test_end   = as.POSIXct("2024-12-31 23:00:00", tz="UTC"))
)

# Features for ML models (subset by availability)
FEATURES <- c(
  "total_occupancy", "co2", "tempC", "humidity", "sound", "lux",
  "temperature", "global_radiation",
  "holiday", "office_hours", "is_weekend",
  "hour_sin", "hour_cos", "weekday",
  "lag_24", "lag_72", "lag_168", "lag_336", "lag_504",
  "rollmean_24", "rollmean_168"
)

# Exogenous for SARIMAX/hybrid (tweak here)
XREG_VARS <- c("total_occupancy", "co2", "lux", "lag_24", "global_radiation", "holiday", "lag_168")

# ===== Helpers =====

prep_split <- function(dt, fold) {
  setorder(dt, interval)
  trn <- dt[interval < fold$test_start]
  tst <- dt[interval >= fold$test_start & interval <= fold$test_end]
  stopifnot(nrow(trn) > 0L, nrow(tst) > 0L)
  list(train = trn, test = tst)
}

safe_head <- function(x, n) if (n <= 0) x[0] else head(x, n)
safe_tail <- function(x, n) if (n <= 0) x[0] else tail(x, n)

# Metrics
eps <- 1e-6
compute_metrics <- function(dt) {
  dt[, residual := actual - forecast]
  dt[, .(
    RMSE  = sqrt(mean(residual^2)),
    MAE   = mean(abs(residual)),
    MAPE  = mean(abs(residual / actual)[abs(actual) > eps]) * 100,  # robust
    sMAPE = mean(200 * abs(residual) / (abs(actual) + abs(forecast) + eps))
  ), by = .(fold, model)]
}

# ===== SARIMA (rolling 24h) =====
run_sarima <- function(train_ts, test_vals, test_dt, fold_name) {
  t0 <- Sys.time()
  h  <- length(test_vals)
  all_fc <- all_y <- numeric(0); all_ts <- as.POSIXct(character(0))
  last_fit <- NULL
  
  for (i in seq(1, h, by = ROLLING_STEPS)) {
    block_end     <- min(i + ROLLING_STEPS - 1L, h)
    current_test  <- test_vals[i:block_end]
    # Extend train with observed test up to i-1 (no leakage)
    y_tr_cur <- ts(c(as.numeric(train_ts), safe_head(test_vals, i-1L)),
                   frequency = SEASONAL_PERIOD)
    fit <- Arima(
      y_tr_cur,
      order = c(1,0,1),
      seasonal = list(order = c(1,1,1), period = SEASONAL_PERIOD),
      include.mean = TRUE,
      method = "CSS"
    )
    last_fit <- fit
    fc <- forecast(fit, h = ROLLING_STEPS)
    cur_fc <- as.numeric(fc$mean)[seq_along(current_test)]
    
    all_fc <- c(all_fc, cur_fc)
    all_y  <- c(all_y,  current_test)
    all_ts <- c(all_ts, test_dt$interval[i:block_end])
  }
  
  runtime <- as.numeric(difftime(Sys.time(), t0, units="secs"))
  list(
    results = data.table(fold = fold_name, interval = all_ts,
                         actual = all_y, forecast = all_fc, model = "SARIMA"),
    last_fit = last_fit, runtime_secs = runtime
  )
}

# ===== SARIMAX (rolling 24h) =====
run_sarimax <- function(train_dt, test_dt, xreg_vars, fold_name) {
  t0 <- Sys.time()
  y_train <- ts(train_dt$total_consumption_kWh, frequency = SEASONAL_PERIOD)
  y_test  <- test_dt$total_consumption_kWh
  X_train <- as.matrix(train_dt[, ..xreg_vars])
  X_test  <- as.matrix(test_dt[, ..xreg_vars])
  h <- length(y_test)
  
  all_fc <- all_y <- numeric(0); all_ts <- as.POSIXct(character(0))
  last_fit <- NULL
  
  for (i in seq(1, h, by = ROLLING_STEPS)) {
    block_end    <- min(i + ROLLING_STEPS - 1L, h)
    current_test <- y_test[i:block_end]
    y_tr_cur     <- ts(c(as.numeric(y_train), safe_head(y_test, i-1L)),
                       frequency = SEASONAL_PERIOD)
    
    X_tr_ext <- rbind(X_train, safe_head(X_test, i-1L))
    X_te_ext <- X_test[i:block_end, , drop = FALSE]
    
    fit <- Arima(
      y_tr_cur,
      order = c(1,0,1),
      seasonal = list(order = c(1,1,1), period = SEASONAL_PERIOD),
      xreg = X_tr_ext,
      include.mean = TRUE,
      method = "CSS"
    )
    last_fit <- fit
    fc <- forecast(fit, h = length(current_test), xreg = X_te_ext)
    
    all_fc <- c(all_fc, as.numeric(fc$mean))
    all_y  <- c(all_y,  current_test)
    all_ts <- c(all_ts, test_dt$interval[i:block_end])
  }
  
  runtime <- as.numeric(difftime(Sys.time(), t0, units="secs"))
  list(
    results = data.table(fold = fold_name, interval = all_ts,
                         actual = all_y, forecast = all_fc, model = "SARIMAX"),
    last_fit = last_fit, runtime_secs = runtime
  )
}

# ===== LSTM =====
run_lstm <- function(train_dt, test_dt, fold_name) {
  t0 <- Sys.time()
  
  feats <- intersect(FEATURES, names(train_dt))
  train_dt[, target := total_consumption_kWh]
  test_dt[,  target := total_consumption_kWh]
  all_data <- rbind(copy(train_dt), copy(test_dt))
  
  rec <- recipe(target ~ ., data = all_data[, c("target", feats), with = FALSE]) %>%
    step_string2factor(all_nominal(), -all_outcomes()) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    step_range(all_numeric(), -all_outcomes()) %>%
    prep(training = train_dt)
  
  norm_train <- bake(rec, train_dt)
  norm_test  <- bake(rec, test_dt)
  
  predictors <- setdiff(names(norm_train), "target")
  X_train <- as.matrix(norm_train[, predictors, drop = FALSE])
  X_test  <- as.matrix(norm_test[,  predictors, drop = FALSE])
  
  y_train <- train_dt$target
  y_test  <- test_dt$target
  
  # target scaling on train only
  y_min <- min(y_train, na.rm = TRUE); y_max <- max(y_train, na.rm = TRUE)
  scale_y  <- function(y) (y - y_min) / (y_max - y_min + 1e-6)
  invert_y <- function(y) y * (y_max - y_min) + y_min
  y_train_s <- scale_y(y_train); y_test_s <- scale_y(y_test)
  
  # sequence builder
  make_seq <- function(X, y, lookback, horizon) {
    n <- nrow(X) - lookback - horizon + 1
    stopifnot(n > 0)
    Xarr <- array(NA_real_, dim = c(n, lookback, ncol(X)))
    Yarr <- array(NA_real_, dim = c(n, horizon))
    for (i in 1:n) {
      Xarr[i,,] <- X[i:(i+lookback-1), ]
      Yarr[i, ] <- y[(i+lookback):(i+lookback+horizon-1)]
    }
    list(X = Xarr, y = Yarr)
  }
  
  train_seq <- make_seq(X_train, y_train_s, LOOKBACK, HORIZON)
  
  # model (Bayes-opt tuned settings you liked)
  units1 <- 70L; units2 <- 63L; dropout <- 0.231; lr <- 0.001950638; batch_sz <- 32L
  
  model <- keras_model_sequential() %>%
    layer_lstm(units = units1, input_shape = c(LOOKBACK, ncol(X_train)), return_sequences = TRUE) %>%
    layer_dropout(rate = dropout) %>%
    layer_lstm(units = units2, return_sequences = FALSE) %>%
    layer_dense(units = HORIZON, activation = "linear")
  
  model %>% compile(optimizer = optimizer_nadam(learning_rate = lr),
                    loss = "mse", metrics = "mae")
  
  history <- model %>% fit(
    x = train_seq$X, y = train_seq$y,
    epochs = 120, batch_size = batch_sz, validation_split = 0.2,
    verbose = 1,
    callbacks = list(
      callback_early_stopping(patience = 10, restore_best_weights = TRUE),
      callback_reduce_lr_on_plateau(factor = 0.5, patience = 5)
    )
  )
  
  # ---- Forecast ENTIRE test (24h rolling), seeding first window with tail(train) ----
  n_test <- nrow(X_test)
  fc <- numeric(0); act <- numeric(0); ts_out <- as.POSIXct(character(0))
  
  for (i in seq(1, n_test, by = HORIZON)) {
    block_end <- min(i + HORIZON - 1L, n_test)
    # rows needed *before* i as lookback
    need_from_test <- min(LOOKBACK, i - 1L)
    need_from_train <- LOOKBACK - need_from_test
    
    hist_mat <- if (need_from_train > 0) {
      rbind(safe_tail(X_train, need_from_train),
            if (need_from_test > 0) X_test[(i - need_from_test):(i - 1L), , drop = FALSE] else NULL)
    } else {
      X_test[(i - need_from_test):(i - 1L), , drop = FALSE]
    }
    
    X_input <- array(hist_mat, dim = c(1, LOOKBACK, ncol(X_test)))
    pred_s  <- predict(model, X_input, verbose = 0)
    pred    <- invert_y(as.numeric(pred_s))[1:(block_end - i + 1L)]
    
    fc  <- c(fc, pred)
    act <- c(act, y_test[i:block_end])
    ts_out <- c(ts_out, test_dt$interval[i:block_end])
  }
  
  runtime <- as.numeric(difftime(Sys.time(), t0, units="secs"))
  list(
    results = data.table(fold = fold_name, interval = ts_out,
                         actual = act, forecast = fc, model = "LSTM"),
    history = history, runtime_secs = runtime
  )
}

# ===== Hybrid: SARIMAX + LSTM(residual) =====
run_hybrid <- function(train_dt, test_dt, xreg_vars, fold_name) {
  t0 <- Sys.time()
  # 1) Fit a single ARIMAX on train to get residuals
  y_tr  <- ts(train_dt$total_consumption_kWh, frequency = SEASONAL_PERIOD)
  X_tr  <- as.matrix(train_dt[, ..xreg_vars])
  base_fit <- Arima(
    y_tr,
    order = c(1,0,1),
    seasonal = list(order = c(1,1,1), period = SEASONAL_PERIOD),
    xreg = X_tr,
    include.mean = TRUE,
    method = "CSS"
  )
  resid_tr <- as.numeric(residuals(base_fit))
  
  # 2) Residual-LSTM using same feature set as main LSTM
  feats <- intersect(FEATURES, names(train_dt))
  train_dt_res <- copy(train_dt); test_dt_res <- copy(test_dt)
  train_dt_res[, resid_target := resid_tr]
  test_dt_res[,  resid_target := NA_real_]
  
  all_res <- rbind(train_dt_res, test_dt_res)
  rec <- recipe(resid_target ~ ., data = all_res[, c("resid_target", feats), with = FALSE]) %>%
    step_string2factor(all_nominal(), -all_outcomes()) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    step_range(all_numeric(), -all_outcomes()) %>%
    prep(training = train_dt_res)
  
  norm_tr <- bake(rec, train_dt_res); norm_te <- bake(rec, test_dt_res)
  predictors <- setdiff(names(norm_tr), "resid_target")
  Xtr <- as.matrix(norm_tr[, predictors, drop = FALSE])
  Xte <- as.matrix(norm_te[, predictors, drop = FALSE])
  
  rmin <- min(resid_tr, na.rm = TRUE); rmax <- max(resid_tr, na.rm = TRUE)
  scale_r  <- function(y) (y - rmin) / (rmax - rmin + 1e-6)
  invert_r <- function(y) y * (rmax - rmin) + rmin
  
  resid_tr_s <- scale_r(resid_tr)
  
  make_seq <- function(X, y, lookback, horizon) {
    n <- nrow(X) - lookback - horizon + 1
    stopifnot(n > 0)
    Xarr <- array(NA_real_, dim = c(n, lookback, ncol(X)))
    Yarr <- array(NA_real_, dim = c(n, horizon))
    for (i in 1:n) {
      Xarr[i,,] <- X[i:(i+lookback-1), ]
      Yarr[i, ] <- y[(i+lookback):(i+lookback+horizon-1)]
    }
    list(X = Xarr, y = Yarr)
  }
  seq_tr <- make_seq(Xtr, resid_tr_s, LOOKBACK, HORIZON)
  # 
  # model_r <- keras_model_sequential() %>%
  #   layer_lstm(units = 64, input_shape = c(LOOKBACK, ncol(Xtr)), return_sequences = TRUE) %>%
  #   layer_dropout(rate = 0.2) %>%
  #   layer_lstm(units = 32, return_sequences = FALSE) %>%
  #   layer_dense(units = HORIZON, activation = "linear")
  # 
  # model_r %>% compile(optimizer = optimizer_adam(learning_rate = 0.001),
  #                     loss = "mse")
  # 
  units1  <- 70L
  units2  <- 63L
  dropout <- 0.231
  lr      <- 0.001950638
  batch_sz <- 32L
  n_epochs <- 120L
  
  model_r <- keras_model_sequential() %>%
    layer_lstm(units = units1, input_shape = c(LOOKBACK, ncol(Xtr)), return_sequences = TRUE) %>%
    layer_dropout(rate = dropout) %>%
    layer_lstm(units = units2, return_sequences = FALSE) %>%
    layer_dense(units = HORIZON, activation = "linear")   # same as standalone
  
  model_r %>% compile(
    optimizer = optimizer_nadam(learning_rate = lr),
    loss = "mse",
    metrics = "mae"
  )
  
  
  model_r %>% fit(seq_tr$X, seq_tr$y, epochs = 80, batch_size = 32,
                  validation_split = 0.2, verbose = 0,
                  callbacks = list(callback_early_stopping(patience = 8, restore_best_weights = TRUE)))
  
  # 3) Rolling base SARIMAX forecast across test
  sarimax_roll <- run_sarimax(train_dt, test_dt, xreg_vars, fold_name)
  base_fc <- sarimax_roll$results[model == "SARIMAX"][, .(interval, base = forecast, actual)]
  
  # 4) Residual LSTM forecast across ENTIRE test (24h blocks), seed with tail(Xtr)
  n_test <- nrow(Xte)
  r_fc <- numeric(0); ts_out <- as.POSIXct(character(0))
  
  for (i in seq(1, n_test, by = HORIZON)) {
    block_end <- min(i + HORIZON - 1L, n_test)
    need_from_test <- min(LOOKBACK, i - 1L)
    need_from_train <- LOOKBACK - need_from_test
    
    hist_mat <- if (need_from_train > 0) {
      rbind(safe_tail(Xtr, need_from_train),
            if (need_from_test > 0) Xte[(i - need_from_test):(i - 1L), , drop = FALSE] else NULL)
    } else {
      Xte[(i - need_from_test):(i - 1L), , drop = FALSE]
    }
    
    Xin <- array(hist_mat, dim = c(1, LOOKBACK, ncol(Xte)))
    pred_s <- predict(model_r, Xin, verbose = 0)
    pred_r <- invert_r(as.numeric(pred_s))[1:(block_end - i + 1L)]
    r_fc   <- c(r_fc, pred_r)
    ts_out <- c(ts_out, test_dt$interval[i:block_end])
  }
  
  # 5) Hybrid = base + residual
  hybrid_dt <- merge(base_fc, data.table(interval = ts_out, rhat = r_fc), by = "interval", all.x = TRUE)
  hybrid_dt[, forecast := base + rhat]
  runtime <- as.numeric(difftime(Sys.time(), t0, units="secs"))
  
  list(
    results = data.table(fold = fold_name, interval = hybrid_dt$interval,
                         actual = hybrid_dt$actual, forecast = hybrid_dt$forecast,
                         model = "HYBRID_SARIMAX+LSTM"),
    last_fit = sarimax_roll$last_fit,
    runtime_secs = runtime
  )
}

# ===== Per-fold runner =====
run_fold <- function(dt, fold_name, fold_def) {
  sp <- prep_split(dt, fold_def)
  tr <- sp$train; te <- sp$test
  
  # Run models
  sarima_out  <- run_sarima(ts(tr$total_consumption_kWh, frequency = SEASONAL_PERIOD),
                            te$total_consumption_kWh, te, fold_name)
  sarimax_out <- run_sarimax(tr, te, XREG_VARS, fold_name)
  lstm_out    <- run_lstm(tr, te, fold_name)
  hybrid_out  <- run_hybrid(tr, te, XREG_VARS, fold_name)
  
  # Collect
  results <- rbindlist(list(
    sarima_out$results, sarimax_out$results, lstm_out$results, hybrid_out$results
  ), use.names = TRUE)
  
  runtimes <- data.table(
    fold = fold_name,
    model = c("SARIMA","SARIMAX","LSTM","HYBRID_SARIMAX+LSTM"),
    runtime_secs = c(sarima_out$runtime_secs, sarimax_out$runtime_secs,
                     lstm_out$runtime_secs, hybrid_out$runtime_secs)
  )
  
  # Plots per model (forecast vs actual)
  for (m in unique(results$model)) {
    p <- ggplot(results[model == m], aes(x = interval)) +
      geom_line(aes(y = actual), size = 0.6, color = "black") +
      geom_line(aes(y = forecast), size = 0.6, color = "steelblue") +
      labs(title = sprintf("%s – %s", m, fold_name), x = "Time", y = "kWh") +
      theme_minimal()
    ggsave(sprintf("forecast_%s_%s.png", m, fold_name), p, width = 10, height = 4, dpi = 200)
  }
  
  # Residual checks (use last fit objects)
  png(sprintf("checkresiduals_SARIMA_%s.png", fold_name), width = 1000, height = 800)
  forecast::checkresiduals(sarima_out$last_fit)
  dev.off()
  
  png(sprintf("checkresiduals_SARIMAX_%s.png", fold_name), width = 1000, height = 800)
  forecast::checkresiduals(sarimax_out$last_fit)
  dev.off()
  
  # LSTM learning curve
  hist <- lstm_out$history
  hist_df <- data.frame(
    epoch = seq_along(hist$metrics$loss),
    loss   = as.numeric(hist$metrics$loss),
    val_loss = as.numeric(hist$metrics$val_loss)
  )
  p_hist <- ggplot(hist_df, aes(x = epoch)) +
    geom_line(aes(y = loss), size = 0.7) +
    geom_line(aes(y = val_loss), size = 0.7, linetype = "dashed") +
    labs(title = sprintf("LSTM Learning Curve – %s", fold_name),
         y = "MSE (train / val)", x = "Epoch") +
    theme_minimal()
  ggsave(sprintf("lstm_history_%s.png", fold_name), p_hist, width = 7, height = 4, dpi = 200)
  
  list(results = results, runtimes = runtimes)
}

# ===== MAIN =====
# Expecting your data.frame/data.table: model_data with column 'interval' (POSIXct) and target 'total_consumption_kWh'
stopifnot(exists("model_data"))
dt <- as.data.table(model_data)

all_res <- list(); all_rt <- list()
for (nm in names(FOLDS)) {
  cat("Running fold:", nm, "\n")
  out <- run_fold(dt, nm, FOLDS[[nm]])
  all_res[[nm]] <- out$results
  all_rt[[nm]]  <- out$runtimes
}
all_results <- rbindlist(all_res, use.names = TRUE)
all_runtimes <- rbindlist(all_rt, use.names = TRUE)

# Metrics per fold + model (add runtime)
metrics <- compute_metrics(all_results)
metrics <- merge(metrics, all_runtimes, by = c("fold","model"), all.x = TRUE)
metrics[, runtime_minutes := runtime_secs/60]
print(metrics[order(model, fold)])

# Aggregate over folds (mean)
agg_metrics <- metrics[, .(
  RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE), sMAPE = mean(sMAPE),
  runtime_minutes = sum(runtime_minutes)  # total time across folds
), by = model][order(RMSE)]
print(agg_metrics)

# Combined multi-model overview plots (optional)
p_over <- ggplot(all_results, aes(x = interval, y = actual)) +
  geom_line(size = 0.4, color = "black") +
  geom_line(aes(y = forecast, color = model), size = 0.4) +
  facet_grid(model ~ fold, scales = "free_x") +
  labs(title = "Forecast vs Actual by Model and Fold", x = "Time", y = "kWh") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("overview_forecasts_by_model_fold.png", p_over, width = 12, height = 8, dpi = 200)
print(p_over)
# Residual diagnostics summary
for (m in unique(all_results$model)) {
  cat(sprintf("\n=== Residual diagnostics (all folds) for %s ===\n", m))
  res <- all_results[model == m, actual - forecast]
  cat(sprintf("Mean: %.4f | Std: %.4f | Skew: %.4f | Kurt: %.4f\n",
              mean(res), sd(res), skewness(res), kurtosis(res)))
  jb <- jarque.bera.test(res); lb <- Box.test(res, lag = 24, type = "Ljung-Box")
  cat(sprintf("Jarque-Bera p=%.4g | Ljung-Box p=%.4g\n", jb$p.value, lb$p.value))
}

# ------------ plot all togehter
# --- 1) Prep (order + nice facet order) ---
setorder(all_results, interval)
all_results[, model := factor(
  model,
  levels = c("HYBRID_SARIMAX+LSTM", "LSTM", "SARIMAX", "SARIMA")
)]

# --- 2) Month divider lines (OCT–DEC) ---
library(lubridate)
rng <- range(all_results$interval, na.rm = TRUE)
m_starts <- seq(floor_date(rng[1], "month"), ceiling_date(rng[2], "month"), by = "1 month")
vlines   <- m_starts[m_starts > rng[1] & m_starts < rng[2]]

# --- 3) One continuous timeline per model (no fold facet) ---
library(ggplot2)

p_stitched <- ggplot(all_results, aes(x = interval)) +
  # actual (black)
  geom_line(aes(y = actual), linewidth = 0.35, color = "black") +
  # forecast (colored by model just for legend; within a facet it's constant)
  geom_line(aes(y = forecast, color = model), linewidth = 0.35, alpha = 0.95) +
  # month separators
  geom_vline(xintercept = vlines, linetype = "dashed", linewidth = 0.25, color = "grey45") +
  facet_grid(rows = vars(model), scales = "free_y") +  # one panel per model, full Oct–Dec on x
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  labs(
    title = "Forecast vs Actual — Oct–Dec 2024 (stitched timeline)",
    x = "Time", y = "kWh"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  guides(color = guide_legend(title = NULL, nrow = 1))

ggsave("overview_forecasts_stitched_by_model.png", p_stitched, width = 13, height = 8, dpi = 200)
print(p_stitched)
p_onepanel <- ggplot(all_results, aes(interval)) +
  geom_line(aes(y = actual), linewidth = 0.35, color = "black") +
  geom_line(aes(y = forecast, color = model), linewidth = 0.35, alpha = 0.9) +
  geom_vline(xintercept = vlines, linetype = "dashed", linewidth = 0.25, color = "grey45") +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "Forecast vs Actual — Oct–Dec 2024", x = "Time", y = "kWh") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

print(p_onepanel)


# --- Prep ---
setorder(all_results, interval)
all_results[, model := factor(
  model,
  levels = c("HYBRID SARIMAX+LSTM", "LSTM", "SARIMAX", "SARIMA")
)]

# Month guide lines
rng      <- range(all_results$interval, na.rm = TRUE)
m_starts <- seq(floor_date(rng[1], "month"), ceiling_date(rng[2], "month"), by = "1 month")
vlines   <- m_starts[m_starts > rng[1] & m_starts < rng[2]]

# --- Save one stitched figure per model ---
for (m in levels(all_results$model)) {
  d <- all_results[model == m]
  
  p <- ggplot(d, aes(x = interval)) +
    geom_line(aes(y = actual), linewidth = 0.35, color = "black") +
    geom_line(aes(y = forecast), linewidth = 0.35, color = "blue", linetype = "dashed") +
    geom_vline(xintercept = vlines, linetype = "dashed", linewidth = 0.25, color = "grey45") +
    scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
    labs(title = paste("Forecast vs Actual —", m, " (Oct–Dec 2024)"),
         x = "Time", y = "kWh") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
  
  fn <- sprintf("stitched_forecast_%s.png", gsub("[^A-Za-z0-9_]+", "_", m))
  ggsave(fn, p, width = 12, height = 3.6, dpi = 200)
  print(p)
}

# Compute residuals once
all_results[, residual := actual - forecast]

# Combined residuals figure, faceted by model (stitched OCT–DEC)
p_resid_all <- ggplot(all_results, aes(interval, residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey40") +
  geom_line(aes(color = model), linewidth = 0.3, alpha = 0.9) +
  geom_vline(xintercept = vlines, linetype = "dashed", linewidth = 0.25, color = "grey45") +
  facet_grid(rows = vars(model), scales = "free_y") +
  scale_color_manual(values = model_cols, guide = "none") +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "Residuals (Actual – Forecast) — Oct–Dec 2024",
       x = "Time", y = "Residual (kWh)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave("residuals_stitched_by_model.png", p_resid_all, width = 12.5, height = 8, dpi = 200)

# ---- Residual time-series: faceted + overlayed ----
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(lubridate)
})

stopifnot(exists("all_results"))
setorder(all_results, interval)

# Residuals
if (!"residual" %in% names(all_results))
  all_results[, residual := actual - forecast]

# Consistent model order + colors
all_results[, model := factor(model,
                              levels = c("HYBRID_SARIMAX+LSTM","LSTM","SARIMAX","SARIMA")
)]
if (!exists("model_cols")) {
  model_cols <- c(
    "HYBRID SARIMAX+LSTM" = "#d95f02",
    "LSTM"                = "#66a61e",
    "SARIMAX"             = "#7570b3",
    "SARIMA"              = "#1b9e77"
  )
}

# Month guide lines
rng      <- range(all_results$interval, na.rm = TRUE)
m_starts <- seq(floor_date(rng[1], "month"),
                ceiling_date(rng[2], "month"),
                by = "1 month")
vlines   <- m_starts[m_starts > rng[1] & m_starts < rng[2]]

# ===== 1) Faceted residuals (stitched OCT–DEC) =====
p_resid_faceted <- ggplot(all_results, aes(interval, residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey45") +
  geom_line(aes(color = model), linewidth = 0.35, alpha = 0.95, show.legend = FALSE) +
  geom_vline(xintercept = vlines, linetype = "dashed", linewidth = 0.25, color = "grey65") +
  facet_grid(rows = vars(model), scales = "free_y") +
  scale_color_manual(values = model_cols) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  labs(title = "Residuals (Actual − Forecast) — Oct–Dec 2024",
       x = "Time", y = "Residual (kWh)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))
ggsave("residuals_faceted_by_model.png", p_resid_faceted, width = 12.5, height = 8, dpi = 200)

# ===== 2) Overlayed residuals (one panel, all models) =====
# (Optional) symmetric y-limits around zero for fair comparison:
ylim_sym <- max(abs(all_results$residual), na.rm = TRUE)

p_resid_overlay <- ggplot(all_results, aes(interval, residual, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, color = "grey45") +
  geom_line(linewidth = 0.35, alpha = 0.9) +
  geom_vline(xintercept = vlines, linetype = "dashed", linewidth = 0.25, color = "grey65") +
  scale_color_manual(values = model_cols) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  coord_cartesian(ylim = c(-ylim_sym, ylim_sym)) +  # comment out if you prefer auto-scale
  labs(title = "Residuals (Actual − Forecast) — Oct–Dec 2024 (overlayed)",
       x = "Time", y = "Residual (kWh)", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"))
ggsave("residuals_overlayed_all_models.png", p_resid_overlay, width = 12.5, height = 4.8, dpi = 200)

# Print to viewer if interactive
print(p_resid_faceted)
print(p_resid_overlay)
