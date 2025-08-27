# ================================================================
# SARIMA — Monthly Retrain for Multiple Horizons
# ================================================================
# ------------------------------------------------
sarima_monthly_retrain <- function(train_dt, test_dt, horizon,
                                   seasonal_period = 168,
                                   auto_args = list(
                                     stepwise = TRUE,
                                     approximation = TRUE,
                                     max.p = 3, max.q = 3,
                                     max.P = 2, max.Q = 2,
                                     d = NA, D = NA,
                                     refit_exact = TRUE,
                                     use_fixed = TRUE      # <- speed: reuse coefficients
                                   )) {
  

  all_dt <- rbindlist(list(train_dt, test_dt), use.names = TRUE, fill = TRUE)
  setorder(all_dt, interval)
  
  y_all <- all_dt$total_consumption_kWh
  t_all <- all_dt$interval
  
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
    
    # ---------------- monthly fit (orders + optional exact refit) ----------------
    y_train <- y_all[seq_len(train_end_idx)]
    base_ts <- ts(y_train, frequency = seasonal_period)
    
    fit_fast <- auto.arima(
      y = base_ts,
      seasonal      = TRUE,
      stepwise      = auto_args$stepwise %||% TRUE,
      approximation = auto_args$approximation %||% TRUE,
      max.p = auto_args$max.p %||% 3,
      max.q = auto_args$max.q %||% 3,
      max.P = auto_args$max.P %||% 2,
      max.Q = auto_args$max.Q %||% 2,
      d = auto_args$d %||% NA,
      D = auto_args$D %||% NA
    )
    
    # exact refit on full train (optional)
    if (auto_args$refit_exact %||% TRUE) {
      ord <- arimaorder(fit_fast)
      fit <- Arima(
        base_ts,
        order = ord[1:3],
        seasonal = list(order = ord[4:6], period = seasonal_period),
        include.drift = fit_fast$include.drift %||% FALSE,
        include.mean  = fit_fast$include.mean  %||% FALSE
      )
    } else {
      fit <- fit_fast
    }
    
    # cache spec for fast rolling updates
    ord  <- arimaorder(fit)
    base_order <- ord[1:3]
    seas_order <- ord[4:6]
    fixed_coef <- tryCatch(coef(fit), error = function(e) NULL)
    use_fixed  <- (auto_args$use_fixed %||% TRUE) && is.numeric(fixed_coef)
    
    # ---------------- leak-free rolling origin at every t ----------------
    month_end_all_idx <- max(ts_month_idx_in_all)
    y_prefix <- y_all[seq_len(month_start_all_idx - 1L)]
    y_month  <- y_all[month_start_all_idx:month_end_all_idx]
    M <- length(y_month)
    
    if (horizon == 1L) {
      # one-step-ahead fitted values (shifted by 1)
      upd <- Arima(ts(c(y_train, y_month), frequency = seasonal_period), model = fit)
      fitted_all   <- fitted(upd)
      fitted_month <- tail(fitted_all, M)
      if (M >= 2) preds[idx_month_test[1:(M-1)]] <- fitted_month[2:M]
    } else {
      # For each test row t (1..M), forecast h steps ahead using data up to t
      for (t in seq_len(M)) {
        y_upto <- c(y_prefix, y_month[seq_len(t)])
        
        if (use_fixed) {
          upd <- Arima(
            ts(y_upto, frequency = seasonal_period),
            order = base_order,
            seasonal = list(order = seas_order, period = seasonal_period),
            include.drift = fit$include.drift %||% FALSE,
            include.mean  = fit$include.mean  %||% FALSE,
            fixed = fixed_coef,
            transform.pars = FALSE
          )
        } else {
          # allows re-optimisation (slower, slightly more accurate)
          upd <- Arima(ts(y_upto, frequency = seasonal_period), model = fit)
        }
        
        fc <- forecast(upd, h = horizon)
        preds[idx_month_test[t]] <- as.numeric(fc$mean[horizon])
      }
    }
  }
  
  preds
}


# ------------------------------------------------
# Runner over horizons (uniform logs/plots/outputs)
# ------------------------------------------------
run_sarima_nov_dec_2024 <- function(model_data,
                                    horizons = c(1, 24, 168),
                                    seasonal_period = 168,
                                    auto_args = list(
                                      stepwise = TRUE,
                                      approximation = TRUE,
                                      max.p = 3, max.q = 3,
                                      max.P = 2, max.Q = 2,
                                      d = NA,  D = NA,
                                      refit_exact = TRUE,
                                      use_fixed = TRUE
                                    )) {
  results <- list(); metrics_list <- list(); plots <- list()
  
  # Respect dataset timezone
  tz_used <- try(lubridate::tz(model_data$interval[1]), silent = TRUE)
  if (inherits(tz_used, "try-error") || is.null(tz_used) || tz_used == "") tz_used <- "UTC"
  
  nov_start <- as.POSIXct("2024-11-01 00:00:00", tz = tz_used)
  jan_start <- as.POSIXct("2025-01-01 00:00:00", tz = tz_used)
  
  for (h in horizons) {
    cat(sprintf("\n============= SARIMA | Nov–Dec 2024 | H = %d =============\n", h))
    
    # Build target as lead(h); then slice train/test by calendar
    data_h <- prepare_target(model_data, h)
    data_h <- data.table::as.data.table(data_h)
    
    train_h <- data_h[interval <  nov_start]
    test_h  <- data_h[interval >= nov_start & interval < jan_start]
    
    if (nrow(test_h) == 0L) {
      warning(sprintf("No test rows in Nov–Dec 2024 for h=%d. Skipping.", h))
      next
    }
    if (nrow(train_h) == 0L) {
      warning(sprintf("No training history before Nov 2024 for h=%d. Skipping.", h))
      next
    }
    
    actual_h <- test_h$target  # may be NA if you don't have actuals yet
    
    t0 <- Sys.time()
    pred_h <- sarima_monthly_retrain(
      train_dt = train_h,
      test_dt  = test_h,
      horizon  = h,
      seasonal_period = seasonal_period,
      auto_args = auto_args
    )
    t1 <- Sys.time()
    runtime_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    
    # Metrics (will be NA if you don't have actuals for Nov–Dec)
    m <- eval_metrics(actual_h, pred_h)
    m$runtime_seconds <- runtime_sec
    m$horizon <- h
    metrics_list[[as.character(h)]] <- m
    
    # Plot (no time shift; target is already a lead(h))
    plot_dt <- data.table::data.table(
      target_time = test_h$interval,
      actual      = actual_h,
      pred        = pred_h
    )
    gg <- ggplot2::ggplot(plot_dt, ggplot2::aes(x = target_time)) +
      ggplot2::geom_line(ggplot2::aes(y = pred,   color = "Predicted"), linewidth = 0.8) +
      ggplot2::geom_line(ggplot2::aes(y = actual, color = "Actual"),    linewidth = 0.6, alpha = 0.8, na.rm = TRUE) +
      ggplot2::scale_color_manual(values = c("Actual" = "#1f77b4", "Predicted" = "#d62728")) +
      ggplot2::labs(
        title = sprintf("SARIMA — Forecast for Nov–Dec 2024 (h = %d, season = %d)", h, seasonal_period),
        x = "Time (target timestamp)", y = "Energy consumption (kWh)", color = "Series",
        caption = sprintf("MAE=%.2f | RMSE=%.2f | MAPE=%.2f%% | R²=%.3f | Run=%.1fs",
                          m$MAE, m$RMSE, m$MAPE, m$R2, runtime_sec)
      ) +
      ggplot2::theme_minimal()
    plots[[as.character(h)]] <- gg
    
    results[[as.character(h)]] <- list(
      horizon = h,
      pred = pred_h,
      actual = actual_h,
      test_interval = test_h$interval,
      metrics = m,
      plot = gg
    )
    
    cat(sprintf("Done: SARIMA (Nov–Dec 2024) | h=%d | MAE=%.3f | RMSE=%.3f | R2=%.3f | %.1fs\n",
                h, m$MAE, m$RMSE, m$R2, runtime_sec))
    gc()
  }
  
  metrics_tbl <- data.table::rbindlist(metrics_list, use.names = TRUE, fill = TRUE)
  data.table::setorder(metrics_tbl, horizon)
  list(results = results, metrics = metrics_tbl, plots = plots)
}


# -------------------- RUN (example) --------------------
sarima_out <- run_sarima_for_horizons(
  model_data      = model_data,
  horizons        = c(1, 24, 168),
  seasonal_period = 168
)
print(sarima_out$metrics)
print(sarima_out$plots[["1"]]); print(sarima_out$plots[["24"]]); print(sarima_out$plots[["168"]])

