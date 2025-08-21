# ================================================================
# LSTM — Monthly Retrain with Explicit XRegs for Multiple Horizons
# ================================================================

# ---------------- Vectorized sequence builder ----------------
make_seq_block <- function(baked, timesteps) {
  X <- as.matrix(baked[, setdiff(names(baked), "target"), drop = FALSE])
  y <- baked$target
  n <- nrow(baked)
  s <- n - timesteps
  if (s <= 0) {
    return(list(X = array(0, dim = c(0, timesteps, ncol(X))),
                y = numeric(0)))
  }
  idx <- matrix(
    rep(seq_len(timesteps), each = s) + rep(0:(s - 1), times = timesteps),
    nrow = s
  )
  X3 <- array(X[idx, , drop = FALSE], dim = c(s, timesteps, ncol(X)))
  yv <- y[timesteps + seq_len(s)]
  list(X = X3, y = yv)
}

# ---------------- LSTM model builder ----------------
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

# --------------- Core: monthly retrain LSTM ----------------
lstm_monthly_retrain_xreg <- function(train_dt, test_dt, horizon,
                                      timesteps = 168,
                                      xreg_num, xreg_cat,
                                      units1 = 128, units2 = 64,
                                      dropout = 0.2, epochs = 80,
                                      batch_size = 32, patience = 6,
                                      final_activation = "linear",
                                      verbose = 1) {
  
  all_dt <- rbindlist(list(copy(train_dt), copy(test_dt)), use.names = TRUE)
  setorder(all_dt, interval)
  
  stopifnot(all(c("interval", "target") %in% names(all_dt)))
  stopifnot(all(xreg_num %in% names(all_dt)))
  stopifnot(all(xreg_cat %in% names(all_dt)))
  
  preds <- rep(NA_real_, nrow(test_dt))
  
  test_loc <- copy(test_dt)
  test_loc[, month_id := floor_date(interval, "month")]
  months_in_test <- unique(test_loc$month_id)
  
  for (m in months_in_test) {
    
    idxm <- which(test_loc$month_id == m)
    ts_m <- test_loc$interval[idxm]
    
    month_start <- min(ts_m)
    train_limit <- month_start - seconds(1) - hours(horizon)
    tr_rows     <- all_dt[interval <= train_limit]
    if (nrow(tr_rows) < (timesteps + 200)) next
    
    all_idx   <- match(ts_m, all_dt$interval)
    start_row <- min(all_idx) - timesteps
    end_row   <- max(all_idx) - 1
    if (start_row < 1) next
    slice_rows <- all_dt[start_row:end_row]
    
    keep_cols <- unique(c("target", xreg_num, xreg_cat))
    tr_df <- as.data.frame(tr_rows[, ..keep_cols])
    sl_df <- as.data.frame(slice_rows[, ..keep_cols])
    
    # Recipe on train only
    rec <- recipe(tr_df) |>
      update_role(target, new_role = "outcome") |>
      update_role(all_of(setdiff(keep_cols, "target")), new_role = "predictor") |>
      step_dummy(all_of(xreg_cat), one_hot = TRUE) |>
      step_range(all_numeric(), -all_outcomes(), min = 0, max = 1) |>
      prep()
    
    
    baked_tr <- bake(rec, tr_df)
    baked_sl <- bake(rec, sl_df)
    
    # Target scaling with TRAIN min-max (for inverse transform)
    tmin <- min(tr_df$target, na.rm = TRUE)
    tmax <- max(tr_df$target, na.rm = TRUE)
    rng  <- if (is.finite(tmax - tmin) && (tmax - tmin) > 1e-8) (tmax - tmin) else 1
    baked_tr$target <- (tr_df$target - tmin) / rng
    baked_sl$target <- (sl_df$target - tmin) / rng
    
    # Sequences
    tr <- make_seq_block(baked_tr, timesteps)
    if (length(tr$y) < 64) next
    
    # Train/val split
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
    
    if (verbose > 0) {
      cat(sprintf("\n=== Training LSTM for month %s | samples=%d | features=%d ===\n",
                  format(m, "%Y-%m"), dim(tr$X)[1], dim(tr$X)[3]))
    }
    
    invisible(model |> fit(
      x = X_tr, y = y_tr,
      validation_data = list(X_va, y_va),
      epochs = epochs, batch_size = batch_size,
      verbose = verbose, callbacks = cbs
    ))
    
    # Month sequences to predict
    sl <- make_seq_block(baked_sl, timesteps)
    if (length(sl$y) == 0) next
    
    yh_norm <- as.numeric(model |> predict(sl$X, verbose = 0))
    yh      <- yh_norm * rng + tmin
    
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
run_lstm_for_horizons <- function(model_data, horizons = c(1, 24, 168),
                                  timesteps = 168,
                                  xreg_num, xreg_cat,
                                  units1 = 128, units2 = 64,
                                  dropout = 0.2, epochs = 80,
                                  batch_size = 32, patience = 6,
                                  final_activation = "linear",
                                  verbose = 1) {
  results <- list(); metrics_list <- list(); plots <- list()
  
  for (h in horizons) {
    cat(sprintf("\n====================== LSTM | H = %d ======================\n", h))
    
    data_h <- prepare_target(model_data, h)
    sets_h <- split_data(data_h)
    train_h <- sets_h$train
    test_h  <- sets_h$test
    actual_h <- test_h$target
    
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
    
    m <- eval_metrics(actual_h, pred_h)
    m$runtime_seconds <- runtime_sec
    m$horizon <- h
    metrics_list[[as.character(h)]] <- m
    
    plot_dt <- data.table(
      target_time = test_h$interval + hours(h),
      actual      = actual_h,
      pred        = pred_h
    )
    gg <- ggplot(plot_dt, aes(x = target_time)) +
      geom_line(aes(y = actual, color = "Actual"), linewidth = 0.7) +
      geom_line(aes(y = pred,   color = "LSTM"),   linewidth = 0.7, linetype = "dashed") +
      scale_color_manual(values = c("Actual" = "#1f77b4", "LSTM" = "#d62728")) +
      labs(
        title = sprintf("LSTM (monthly retrain) — h = %d, lookback = %d", h, timesteps),
        x = "Time (target timestamp)", y = "Energy consumption (kWh)", color = "Series",
        caption = sprintf("MAE=%.2f | RMSE=%.2f | MAPE=%.2f%% | R²=%.3f | Run=%.1fs",
                          m$MAE, m$RMSE, m$MAPE, m$R2, runtime_sec)
      ) +
      theme_minimal()
    plots[[as.character(h)]] <- gg
    
    results[[as.character(h)]] <- list(
      horizon = h, pred = pred_h, actual = actual_h,
      test_interval = test_h$interval, metrics = m, plot = gg
    )
    
    cat(sprintf("Done: LSTM | h=%d | MAE=%.3f | RMSE=%.3f | R2=%.3f | %.1fs\n",
                h, m$MAE, m$RMSE, m$R2, runtime_sec))
    gc()
  }
  
  metrics_tbl <- rbindlist(metrics_list, use.names = TRUE, fill = TRUE)
  setorder(metrics_tbl, horizon)
  list(results = results, metrics = metrics_tbl, plots = plots)
}

# -------------------- RUN (example) --------------------
timesteps <- 168
# when defining your regressors:
xreg_num <- c("total_occupancy","co2","tempC","humidity","sound","lux",
              "temperature","global_radiation","wind_speed",
              "lag_24","lag_72","lag_168","lag_336","rollmean_168",
              "hour_sin")   # strictly numeric only

xreg_cat <- c("weekday","month","holiday")   # factor/categorical → dummies

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
