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
                                     refit_exact = TRUE
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
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  for (m in months_in_test) {
    idx_month_test      <- which(test_loc$month_id == m)
    ts_month_idx_in_all <- idx_in_all[idx_month_test]
    month_start_all_idx <- min(ts_month_idx_in_all)
    train_end_idx <- month_start_all_idx - 1L
    if (train_end_idx < 1L) next
    
    y_train <- y_all[seq_len(train_end_idx)]
    base_ts <- ts(y_train, frequency = seasonal_period)
    
    # Fast auto.arima search
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
    
    # Optional exact refit using selected orders
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
    
    # Forecast logic differs for h==1 vs h>1
    month_end_all_idx <- max(ts_month_idx_in_all)
    y_prefix <- y_all[seq_len(month_start_all_idx - 1L)]
    y_month  <- y_all[month_start_all_idx:month_end_all_idx]
    M <- length(y_month)
    
    if (horizon == 1L) {
      upd <- Arima(ts(c(y_train, y_month), frequency = seasonal_period), model = fit)
      fitted_all <- fitted(upd)
      fitted_month <- tail(fitted_all, M)
      if (M >= 2) {
        preds[idx_month_test[1:(M-1)]] <- fitted_month[2:M]
      }
    } else {
      for (j in seq(1L, M, by = horizon)) {
        # Use info up to j-1 (not including j)
        y_upto <- c(y_prefix, if (j > 1L) y_month[seq_len(j - 1L)] else numeric(0))
        upd <- Arima(ts(y_upto, frequency = seasonal_period), model = fit)
        
        fc  <- forecast(upd, h = horizon)
        fc_mean <- as.numeric(fc$mean)
        
        block_len <- min(horizon, M - j + 1L)   # number of *future* points we produced
        # For k in 1..block_len, fc_mean[k] is for time (j + k)
        # We want pred[t] to equal y[t + h], so t = j + k - h
        for (k in seq_len(block_len)) {
          t_rel <- j + k - horizon
          if (t_rel >= 1L) {
            preds[idx_month_test[t_rel]] <- fc_mean[k]
          }
        }
      }
    }
    
  }
  
  preds
}

# ------------------------------------------------
# Runner over horizons (uniform logs/plots/outputs)
# ------------------------------------------------
run_sarima_for_horizons <- function(model_data,
                                    horizons = c(1, 24, 168),
                                    seasonal_period = 168,
                                    auto_args = list(
                                      stepwise = TRUE,
                                      approximation = TRUE,
                                      max.p = 3, max.q = 3,
                                      max.P = 2, max.Q = 2,
                                      d = NA,  D = NA,
                                      refit_exact = TRUE
                                    )) {
  results <- list(); metrics_list <- list(); plots <- list()
  
  for (h in horizons) {
    cat(sprintf("\n===================== SARIMA | H = %d =====================\n", h))
    
    data_h <- prepare_target(model_data, h)
    sets_h <- split_data(data_h)
    train_h <- sets_h$train
    test_h  <- sets_h$test
    actual_h <- test_h$target
    
    t0 <- Sys.time()
    pred_h <- sarima_monthly_retrain(
      train_dt = train_h,
      test_dt = test_h,
      horizon = h,
      seasonal_period = seasonal_period,
      auto_args = auto_args
    )
    t1 <- Sys.time()
    runtime_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    
    m <- eval_metrics(actual_h, pred_h)
    m$runtime_seconds <- runtime_sec
    m$horizon <- h
    metrics_list[[as.character(h)]] <- m
    
    plot_dt <- data.table(
      target_time = test_h$interval,
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
    
    results[[as.character(h)]] <- list(
      horizon = h, pred = pred_h, actual = actual_h,
      test_interval = test_h$interval, metrics = m, plot = gg
    )
    
    cat(sprintf("Done: SARIMA | h=%d | MAPE=%.3f | R2=%.3f | %.1fs\n",
                h, m$MAPE, m$R2, runtime_sec))
    gc()
  }
  
  metrics_tbl <- rbindlist(metrics_list, use.names = TRUE, fill = TRUE)
  setorder(metrics_tbl, horizon)
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

