# ================================================================
# LSTM — Monthly Retrain with Explicit XRegs for Multiple Horizons
# ================================================================

options(digits = 7, OutDec = ".", keras.view_metrics = FALSE)  # safe number formatting & disable viewer
Sys.setlocale("LC_NUMERIC", "C")
fmt_month <- function(x) strftime(as.POSIXct(x), "%Y-%m", tz = "UTC")

  # Coerce categorical columns to factors
  coerce_cats <- function(df, cat_cols) {
    for (cc in intersect(cat_cols, names(df))) {
      df[[cc]] <- factor(as.character(df[[cc]]))
    }
    df
  }

# Vectorized sequence builder: returns (samples, timesteps, features) and y
make_seq_block <- function(baked, timesteps) {
  X <- as.matrix(baked[, setdiff(names(baked), "target"), drop = FALSE])
  y <- baked$target
  n <- nrow(baked)
  s <- n - timesteps
  if (s <= 0) {
    return(list(X = array(0, dim = c(0, timesteps, ncol(X))), y = numeric(0)))
  }
  # Build row indices for each sliding window
  idx <- matrix(
    rep(seq_len(timesteps), each = s) + rep(0:(s - 1), times = timesteps),
    nrow = s
  )
  X3 <- array(X[idx, , drop = FALSE], dim = c(s, timesteps, ncol(X)))
  yv <- y[timesteps + seq_len(s)]
  list(X = X3, y = yv)
}

# Build/compile the LSTM
build_lstm <- function(input_shape,
                       units1 = 128,
                       units2 = 64,
                       dropout = 0.2,
                       lr = 1e-3,
                       act = "linear") {
  keras_model_sequential() |>
    layer_lstm(units = units1, return_sequences = TRUE, input_shape = input_shape) |>
    layer_dropout(dropout) |>
    layer_lstm(units = units2) |>
    layer_dropout(dropout) |>
    layer_dense(units = 1, activation = act) |>
    compile(optimizer = optimizer_adam(lr), loss = "mse", metrics = "mae")
}

# ---------------- LSTM with monthly retrain & explicit xregs ----------------
lstm_monthly_retrain_xreg <- function(train_dt, test_dt, horizon,
                                      timesteps = 168,
                                      xreg_num, xreg_cat,
                                      units1 = 128, units2 = 64,
                                      dropout = 0.2, epochs = 80,
                                      batch_size = 32, patience = 6,
                                      final_activation = "linear",
                                      verbose = 1) {
  
  all_dt <- rbindlist(list(copy(train_dt), copy(test_dt)), use.names = TRUE, fill = TRUE)
  setorder(all_dt, interval)
  
  stopifnot(all(c("interval", "target") %in% names(all_dt)))
  
  # Keep only regressors that exist; warn about dropped ones
  want_num <- xreg_num
  want_cat <- xreg_cat
  xreg_num <- intersect(xreg_num, names(all_dt))
  xreg_cat <- intersect(xreg_cat, names(all_dt))
  dropped  <- setdiff(c(want_num, want_cat), c(xreg_num, xreg_cat))
  if (length(dropped)) {
    message("Dropping missing regressors: ", paste(dropped, collapse = ", "))
  }
  
  # Pre-allocate preds for test rows
  preds <- rep(NA_real_, nrow(test_dt))
  
  # Monthly loop over test
  test_loc <- copy(test_dt)
  test_loc[, month_id := floor_date(interval, "month")]
  months_in_test <- unique(test_loc$month_id)
  
  for (m in months_in_test) {
    
    idxm <- which(test_loc$month_id == m)
    ts_m <- test_loc$interval[idxm]
    
    # We must train using info available strictly before targets:
    # month start minus (horizon hours) gives last usable training timestamp
    month_start <- min(ts_m)
    train_limit <- month_start - seconds(1) - hours(horizon)
    tr_rows     <- all_dt[interval <= train_limit]
    if (nrow(tr_rows) < (timesteps + 200)) next  # not enough data
    
    # Build a continuous slice ending at the last needed context
    all_idx   <- match(ts_m, all_dt$interval)
    start_row <- min(all_idx) - timesteps
    end_row   <- max(all_idx) - 1L
    if (start_row < 1) next
    slice_rows <- all_dt[start_row:end_row]
    
    keep_cols <- unique(c("target", xreg_num, xreg_cat))
    tr_df <- as.data.frame(tr_rows[, ..keep_cols])
    sl_df <- as.data.frame(slice_rows[, ..keep_cols])
    
    # Ensure categoricals are factors in both sets
    tr_df <- coerce_cats(tr_df, xreg_cat)
    sl_df <- coerce_cats(sl_df, xreg_cat)
    
    # Recipe on TRAIN only (no leakage); robust to types
    rec <- recipe(tr_df) |>
      update_role(target, new_role = "outcome") |>
      update_role(all_of(setdiff(keep_cols, "target")), new_role = "predictor") |>
      step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
      step_zv(all_predictors()) |>
      step_range(all_numeric_predictors(), min = 0, max = 1) |>
      prep()
    
    baked_tr <- bake(rec, tr_df)
    baked_sl <- bake(rec, sl_df)
    
    # Scale target with TRAIN min-max (for inverse transform)
    tmin <- min(tr_df$target, na.rm = TRUE)
    tmax <- max(tr_df$target, na.rm = TRUE)
    rng  <- if (is.finite(tmax - tmin) && (tmax - tmin) > 1e-8) (tmax - tmin) else 1
    baked_tr$target <- (tr_df$target - tmin) / rng
    baked_sl$target <- (sl_df$target - tmin) / rng
    
    # Build sequences on TRAIN
    tr <- make_seq_block(baked_tr, timesteps)
    if (length(tr$y) < 64) next  # too few samples
    
    # Train/validation split (chronological)
    n     <- dim(tr$X)[1]
    n_val <- max(1, floor(0.2 * n))
    n_tr  <- n - n_val
    X_tr <- tr$X[1:n_tr, , , drop = FALSE]
    y_tr <- tr$y[1:n_tr]
    X_va <- tr$X[(n_tr + 1):n, , , drop = FALSE]
    y_va <- tr$y[(n_tr + 1):n]
    
    model <- build_lstm(
      input_shape = c(timesteps, dim(tr$X)[3]),
      units1 = units1, units2 = units2,
      dropout = dropout, lr = 1e-3, act = final_activation
    )
    
    cbs <- list(
      callback_early_stopping(monitor = "val_loss",
                              patience = patience, min_delta = 1e-4,
                              restore_best_weights = TRUE),
      callback_reduce_lr_on_plateau(monitor = "val_loss",
                                    factor = 0.5,
                                    patience = max(2, floor(patience / 2)),
                                    min_lr = 1e-5,
                                    verbose = as.integer(verbose > 0))
    )
    
    # Safe banner (avoid base::format ambiguity)
    if (verbose > 0) {
      cat(sprintf("\n=== Training LSTM for month %s | samples=%d | features=%d ===\n",
                  fmt_month(m), dim(tr$X)[1], dim(tr$X)[3]))
    }
    
    # Silence Keras progress formatter (the usual culprit)
    invisible(model |> fit(
      x = X_tr, y = y_tr,
      validation_data = list(X_va, y_va),
      epochs = epochs, batch_size = batch_size,
      verbose = 0,                    # << turn off bar/metrics printing
      callbacks = cbs
    ))
    
  
    
    # Predict over the month slice (context windows slide across slice_rows)
    sl <- make_seq_block(baked_sl, timesteps)
    if (length(sl$y) == 0) next
    
    yh_norm <- as.numeric(model |> predict(sl$X, verbose = 0))
    yh      <- yh_norm * rng + tmin
    
    # Map each sequence end time back to its timestamp and fill preds
    end_times <- slice_rows$interval[(timesteps + 1):nrow(slice_rows)]
    mtch <- match(ts_m, end_times)
    ok   <- which(!is.na(mtch))
    preds[idxm[ok]] <- yh[mtch[ok]]
  }
  
  preds
}

# ------------------------------------------------
# Runner over horizons (uniform logs/plots/outputs)
# ------------------------------------------------
# ------------------------------------------------
# Runner: LSTM for ONLY Nov–Dec 2024
# ------------------------------------------------
run_lstm_nov_dec_2024 <- function(model_data, horizons = c(1, 24, 168),
                                  timesteps = 168,
                                  xreg_num, xreg_cat,
                                  units1 = 128, units2 = 64,
                                  dropout = 0.2, epochs = 80,
                                  batch_size = 32, patience = 6,
                                  final_activation = "linear",
                                  verbose = 1) {
  results <- list(); metrics_list <- list(); plots <- list()
  
  dt <- data.table::as.data.table(model_data)
  data.table::setorder(dt, interval)
  
  # Use dataset timezone if present
  tz_used <- try(lubridate::tz(dt$interval[1]), silent = TRUE)
  if (inherits(tz_used, "try-error") || is.null(tz_used) || tz_used == "") tz_used <- "UTC"
  
  nov_start <- as.POSIXct("2024-11-01 00:00:00", tz = tz_used)
  jan_start <- as.POSIXct("2025-01-01 00:00:00", tz = tz_used)
  
  for (h in horizons) {
    cat(sprintf("\n================ LSTM (Nov–Dec 2024) | H = %d ================\n", h))
    
    # prepare lead(h) target, then slice by calendar window
    data_h <- prepare_target(dt, h)
    data_h <- data.table::as.data.table(data_h)
    
    train_h <- data_h[interval <  nov_start]
    test_h  <- data_h[interval >= nov_start & interval < jan_start]
    
    if (nrow(test_h) == 0L) {
      warning(sprintf("No test rows in Nov–Dec 2024 for h=%d. Skipping.", h))
      next
    }
    if (nrow(train_h) < (timesteps + 200)) {
      warning(sprintf("Not enough pre-Nov history to train LSTM (h=%d). Skipping.", h))
      next
    }
    
    actual_h <- test_h$target  # may be NA if you don't have actuals yet
    
    t0 <- Sys.time()
    pred_h <- lstm_monthly_retrain_xreg(
      train_dt = train_h,
      test_dt  = test_h,
      horizon  = h,
      timesteps = timesteps,
      xreg_num = xreg_num,
      xreg_cat = xreg_cat,
      units1 = units1, units2 = units2,
      dropout = dropout, epochs = epochs,
      batch_size = batch_size, patience = patience,
      final_activation = final_activation,
      verbose = verbose
    )
    t1 <- Sys.time()
    runtime_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    
    # Metrics (NA-safe if actuals missing)
    m <- eval_metrics(actual_h, pred_h)
    m$runtime_seconds <- runtime_sec
    m$horizon <- h
    metrics_list[[as.character(h)]] <- m
    
    # Plot (target already lead(h), so no +hours(h))
    plot_dt <- data.table::data.table(
      target_time = test_h$interval,
      actual      = actual_h,
      pred        = pred_h
    )
    gg <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = target_time)) +
      ggplot2::geom_line(ggplot2::aes(y = actual, color = "Actual"), linewidth = 0.7, na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(y = pred,   color = "LSTM"),   linewidth = 0.7, linetype = "dashed") +
      ggplot2::scale_color_manual(values = c("Actual" = "#1f77b4", "LSTM" = "#d62728")) +
      ggplot2::labs(
        title = sprintf("LSTM (monthly retrain) — Nov–Dec 2024 | h=%d, lookback=%d", h, timesteps),
        x = "Time (target timestamp)", y = "Energy consumption (kWh)", color = "Series",
        caption = sprintf("MAE=%.2f | RMSE=%.2f | MAPE=%.2f%% | R²=%.3f | Run=%.1fs",
                          m$MAE, m$RMSE, m$MAPE, m$R2, runtime_sec)
      ) +
      ggplot2::theme_minimal()
    plots[[as.character(h)]] <- gg
    
    results[[as.character(h)]] <- list(
      horizon = h, pred = pred_h, actual = actual_h,
      test_interval = test_h$interval, metrics = m, plot = gg
    )
    
    cat(sprintf("Done: LSTM (Nov–Dec 2024) | h=%d | MAE=%.3f | RMSE=%.3f | R2=%.3f | %.1fs\n",
                h, m$MAE, m$RMSE, m$R2, runtime_sec))
    gc()
  }
  
  metrics_tbl <- data.table::rbindlist(metrics_list, use.names = TRUE, fill = TRUE)
  data.table::setorder(metrics_tbl, horizon)
  list(results = results, metrics = metrics_tbl, plots = plots)
}


# ================================================================
# EXAMPLE RUN
# ================================================================
# timesteps should cover the dominant seasonality (e.g., 168 for weekly)
timesteps <- 168

# Define your regressors (use what exists in model_data; missing ones are dropped)
xreg_num <- c(
  "total_occupancy","co2","tempC","humidity","sound","lux",
  "temperature","global_radiation","wind_speed",
  "lag_24","lag_72","lag_168","lag_336","rollmean_168",
  "hour_sin"  # strictly numeric only
)
xreg_cat <- c("weekday","month","holiday")  # will be one-hot encoded

# Run (assumes you already have `model_data` with columns used above)
lstm_out <- run_lstm_for_horizons(
  model_data = model_data,
  horizons   = c(1, 24, 168),
  timesteps  = timesteps,
  xreg_num   = xreg_num,
  xreg_cat   = xreg_cat,
  units1     = 128, units2 = 64,
  dropout    = 0.2, epochs = 80, batch_size = 32, patience = 6,
  final_activation = "linear",
  verbose = 1
)

print(lstm_out$metrics)
print(lstm_out$plots[["1"]]); print(lstm_out$plots[["24"]]); print(lstm_out$plots[["168"]])
