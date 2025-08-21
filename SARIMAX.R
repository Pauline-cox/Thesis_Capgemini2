# ================================================================
# SARIMAX — Monthly Retrain (Fast xreg Design Matrix) for Multi Horizons
# ================================================================

# ---------- Build xreg design matrix for ALL rows ----------
.make_xreg_all <- function(all_dt, xreg_cols) {
  dt <- copy(all_dt)
  
  # Categorical columns to dummy encode
  cat_cols  <- intersect(c("weekday", "month"), xreg_cols)
  cont_cols <- setdiff(xreg_cols, cat_cols)
  
  # Fill NA for continuous cols
  if (length(cont_cols)) {
    for (cc in cont_cols) {
      stopifnot(cc %in% names(dt))
      v <- dt[[cc]]
      if (!is.numeric(v)) v <- as.numeric(v)
      v <- data.table::nafill(v, type = "locf")
      v <- data.table::nafill(v, type = "nocb")
      v[!is.finite(v)] <- 0
      dt[[cc]] <- v
    }
  }
  
  X_cont <- if (length(cont_cols)) as.matrix(dt[, ..cont_cols]) else NULL
  if (!is.null(X_cont)) storage.mode(X_cont) <- "numeric"
  
  # One-hot encode categorical cols
  X_cat <- NULL
  if (length(cat_cols)) {
    for (cc in cat_cols) dt[[cc]] <- factor(dt[[cc]])
    X_cat <- model.matrix(~ . - 1, data = dt[, ..cat_cols], na.action = na.pass)
    X_cat[!is.finite(X_cat)] <- 0
  }
  
  # Combine continuous + categorical
  if (is.null(X_cont)) {
    X_all <- X_cat
  } else if (is.null(X_cat)) {
    X_all <- X_cont
  } else {
    X_all <- cbind(X_cont, X_cat)
  }
  
  storage.mode(X_all) <- "numeric"
  X_all
}


# ---------- Core: monthly-retrain SARIMAX (fast) ----------
sarimax_monthly_retrain_fast <- function(train_dt, test_dt, horizon,
                                         xreg_cols,
                                         seasonal_period = 168,
                                         auto_args = list(
                                           stepwise = TRUE,
                                           approximation = TRUE,
                                           max.p = 3, max.q = 3,
                                           max.P = 2, max.Q = 2,
                                           d = NA, D = NA
                                         )) {
  
  all_dt <- rbindlist(list(copy(train_dt), copy(test_dt)), use.names = TRUE, fill = TRUE)
  setorder(all_dt, interval)
  
  y_all <- all_dt$total_consumption_kWh
  t_all <- all_dt$interval
  X_all <- .make_xreg_all(all_dt, xreg_cols)
  
  test_loc <- copy(test_dt)
  test_loc[, month_id := floor_date(interval, "month")]
  months_in_test <- unique(test_loc$month_id)
  
  preds <- rep(NA_real_, nrow(test_dt))
  idx_in_all <- match(test_dt$interval, t_all)
  
  for (m in months_in_test) {
    idx_month_test      <- which(test_loc$month_id == m)
    ts_month_idx_in_all <- idx_in_all[idx_month_test]
    
    month_start_all_idx <- min(ts_month_idx_in_all)
    train_end_idx <- month_start_all_idx - 1L
    if (train_end_idx < 1L) next
    
    y_train <- y_all[seq_len(train_end_idx)]
    X_train <- X_all[seq_len(train_end_idx), , drop = FALSE]
    base_ts <- ts(y_train, frequency = seasonal_period)
    
    fit_fast <- auto.arima(
      y = base_ts,
      xreg          = X_train,
      seasonal      = TRUE,
      stepwise      = auto_args$stepwise %||% TRUE,
      approximation = auto_args$approximation %||% TRUE,
      max.p = auto_args$max.p %||% 3,
      max.q = auto_args$max.q %||% 3,
      max.P = auto_args$max.P %||% 2,
      max.Q = auto_args$max.Q %||% 2,
      d     = auto_args$d     %||% NA,
      D     = auto_args$D     %||% NA
    )
    
    ord <- arimaorder(fit_fast)
    p <- ord[1]; d <- ord[2]; q <- ord[3]
    P <- ifelse(is.na(ord[4]), 0L, ord[4])
    D <- ifelse(is.na(ord[5]), 0L, ord[5])
    Q <- ifelse(is.na(ord[6]), 0L, ord[6])
    
    fit <- tryCatch(
      Arima(
        y       = base_ts,
        order   = c(p, d, q),
        seasonal = list(order = c(P, D, Q), period = seasonal_period),
        include.drift = fit_fast$include.drift %||% FALSE,
        include.mean  = fit_fast$include.mean  %||% FALSE,
        xreg = X_train
      ),
      error = function(e) {
        message("Exact refit failed: ", e$message)
        fit_fast
      }
    )
    
    for (k in seq_along(idx_month_test)) {
      origin_all_idx <- ts_month_idx_in_all[k]
      h_eff <- min(horizon, nrow(all_dt) - origin_all_idx)
      if (h_eff < 1L) { preds[idx_month_test[k]] <- NA_real_; next }
      
      y_upto <- y_all[seq_len(origin_all_idx)]
      X_upto <- X_all[seq_len(origin_all_idx), , drop = FALSE]
      X_fut  <- X_all[(origin_all_idx + 1):(origin_all_idx + h_eff), , drop = FALSE]
      
      upd <- Arima(ts(y_upto, frequency = seasonal_period),
                   xreg = X_upto,
                   model = fit)
      
      fc <- forecast(upd, h = h_eff, xreg = X_fut)
      preds[idx_month_test[k]] <- as.numeric(fc$mean[h_eff])
    }
  }
  
  preds
}

# ------------------------------------------------
# Runner over horizons (uniform logs/plots/outputs)
# ------------------------------------------------
run_sarimax_for_horizons <- function(model_data,
                                     horizons = c(1, 24, 168),
                                     xreg_cols,
                                     seasonal_period = 168,
                                     auto_args = list(
                                       stepwise = TRUE,
                                       approximation = TRUE,
                                       max.p = 3, max.q = 3,
                                       max.P = 2, max.Q = 2,
                                       d = NA, D = NA
                                     )) {
  results <- list(); metrics_list <- list(); plots <- list()
  
  for (h in horizons) {
    cat(sprintf("\n==================== SARIMAX | H = %d ====================\n", h))
    
    data_h <- prepare_target(model_data, h)
    sets_h <- split_data(data_h)
    train_h <- sets_h$train
    test_h  <- sets_h$test
    actual_h <- test_h$target
    
    t0 <- Sys.time()
    pred_h <- sarimax_monthly_retrain_fast(
      train_dt        = train_h,
      test_dt         = test_h,
      horizon         = h,
      xreg_cols       = xreg_cols,
      seasonal_period = seasonal_period,
      auto_args       = auto_args
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
      geom_line(aes(y = pred,   color = "Predicted"), linewidth = 0.7, linetype = "dashed") +
      scale_color_manual(values = c("Actual" = "#1f77b4", "Predicted" = "#d62728")) +
      labs(
        title   = sprintf("SARIMAX (monthly retrain, fast) — h = %d, season = %d", h, seasonal_period),
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
    
    cat(sprintf("Done: SARIMAX | h=%d | MAE=%.3f | RMSE=%.3f | R2=%.3f | %.1fs\n",
                h, m$MAE, m$RMSE, m$R2, runtime_sec))
    gc()
  }
  
  metrics_tbl <- rbindlist(metrics_list, use.names = TRUE, fill = TRUE)
  setorder(metrics_tbl, horizon)
  list(results = results, metrics = metrics_tbl, plots = plots)
}

# -------------------- RUN (example) --------------------
xreg_cols <- c("total_occupancy","co2","tempC","humidity","sound","lux",
               "temperature","global_radiation","wind_speed",
               "lag_24","lag_72","lag_168","lag_336","rollmean_168",
               "hour_sin","weekday","month","holiday")

sarimax_out <- run_sarimax_for_horizons(
  model_data      = model_data,
  horizons        = c(1, 24, 168),
  xreg_cols       = xreg_cols,
  seasonal_period = 168
)
print(sarimax_out$metrics)
print(sarimax_out$plots[["1"]]); print(sarimax_out$plots[["24"]]); print(sarimax_out$plots[["168"]])
