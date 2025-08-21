library(data.table)
library(lubridate)
library(forecast)
library(ggplot2)

# ------------------------------------------------------------------
# Run SARIMA monthly-retrain for multiple horizons
# ------------------------------------------------------------------
run_sarima_for_horizons <- function(model_data,
                                    horizons = c(1, 24, 168),
                                    seasonal_period = 24,   # 24 (daily) of 168 (wekelijks)
                                    auto_args = list(
                                      stepwise = TRUE,
                                      approximation = TRUE,   # (3) sneller zoeken
                                      max.p = 3, max.q = 3,
                                      max.P = 2, max.Q = 2,
                                      d = NA,  D = NA,        # laat differencing vrij (pas aan indien bekend)
                                      refit_exact = TRUE      # na selectie exact herfitten (kan ook FALSE)
                                    )) {
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
  auto_args        = list(
    stepwise = TRUE,
    approximation = TRUE,            # (3) sneller zoeken
    max.p = 3, max.q = 3,
    max.P = 2, max.Q = 2,
    d = NA, D = NA,
    refit_exact = TRUE
  )
)

# Comparison table
print(sarima_out$metrics)

# Show plots (per horizon)
print(sarima_out$plots[["1"]])
print(sarima_out$plots[["24"]])
print(sarima_out$plots[["168"]])


# ---------- (1) & (2) & (3): Versnelde monthly re-train SARIMA ----------
sarima_monthly_retrain <- function(train_dt, test_dt, horizon,
                                   seasonal_period = 24,
                                   auto_args = list(
                                     stepwise = TRUE,
                                     approximation = TRUE,
                                     max.p = 3, max.q = 3,
                                     max.P = 2, max.Q = 2,
                                     d = NA, D = NA,
                                     refit_exact = TRUE
                                   )) {
  # Eén keer binden en sorteren
  all_dt <- rbindlist(list(train_dt, test_dt), use.names = TRUE, fill = TRUE)
  setorder(all_dt, interval)
  
  # Pre-haal pure vector & index op (scheelt veel)
  y_all <- all_dt$total_consumption_kWh
  t_all <- all_dt$interval
  
  # Id maanden in test
  test_dt <- copy(test_dt)
  test_dt[, month_id := floor_date(interval, "month")]
  months_in_test <- unique(test_dt$month_id)
  
  # Opslag voor predictions (in volgorde van test_dt)
  preds <- rep(NA_real_, nrow(test_dt))
  
  # Handige map: voor snelle slicing via integer-indices
  idx_in_all <- match(test_dt$interval, t_all)
  
  for (m in months_in_test) {
    # indices van test-rows in deze maand
    idx_month_test <- which(test_dt$month_id == m)
    ts_month_idx_in_all <- idx_in_all[idx_month_test]
    
    # Train tot net vóór de maand
    month_start_all_idx <- min(ts_month_idx_in_all)
    train_end_idx <- month_start_all_idx - 1L
    if (train_end_idx < 1L) next  # veiligheid
    
    y_train <- y_all[seq_len(train_end_idx)]
    
    # (3) Snelle modelselectie met beperkte zoekruimte
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
    
    # Optioneel exact herfitten met gevonden ordes (sneller dan auto zoeken)
    if (auto_args$refit_exact %||% TRUE) {
      ord <- arimaorder(fit_fast)
      fit <- Arima(base_ts,
                   order = ord[1:3],
                   seasonal = list(order = ord[4:6], period = seasonal_period),
                   include.drift = fit_fast$include.drift %||% FALSE,
                   include.mean  = fit_fast$include.mean  %||% FALSE)
    } else {
      fit <- fit_fast
    }
    
    # ------------------ (1) h == 1: één update per maand ------------------
    if (horizon == 1L) {
      # Pak observaties binnen de maand (op ALL-index)
      month_end_all_idx <- max(ts_month_idx_in_all)
      y_month <- y_all[month_start_all_idx:month_end_all_idx]
      
      # Eén state-update over train + hele maand
      upd <- Arima(ts(c(y_train, y_month), frequency = seasonal_period), model = fit)
      
      # 1-step-ahead in-sample voorspellingen voor de maand
      fitted_all <- fitted(upd)
      # Alleen het deel dat overlapt met y_month
      fitted_month <- tail(fitted_all, length(y_month))
      
      # Align: origin t -> target t+1 => gebruik fitted op (j+1)
      # Laat de laatste origin van de maand open; die wordt gevuld bij de volgende maand
      M <- length(idx_month_test)
      if (M >= 2) {
        preds[idx_month_test[1:(M-1)]] <- fitted_month[2:M]
      }
      # De laatste (M) wordt in de volgende maand gevuld (of blijft NA als er geen data meer is)
      
    } else {
      # ------------------ (2) h > 1: stride = h ------------------
      # We doen één update per h stappen en vullen het volgende blok
      # met de h-staps forecast-pad. Dit benadert rolling-origin sterk,
      # maar is O(M/h) ipv O(M).
      
      # We hebben de volledige y tot en met elk origin nodig; pak de maandobservaties
      month_end_all_idx <- max(ts_month_idx_in_all)
      y_prefix <- y_all[seq_len(month_start_all_idx - 1L)]  # tot vóór maand
      y_month  <- y_all[month_start_all_idx:month_end_all_idx]
      M <- length(y_month)
      
      # Loop in blokken van grootte h
      for (j in seq(1L, M, by = horizon)) {
        # origin positie in maand (j) correspondeert met test_idx binnen de maand
        # NB: test_dt heeft origins exact op deze posities
        # Update state t/m het origin-punt j
        y_upto <- c(y_prefix, y_month[seq_len(j)])
        upd <- Arima(ts(y_upto, frequency = seasonal_period), model = fit)
        fc  <- forecast(upd, h = horizon)
        fc_mean <- as.numeric(fc$mean)
        
        # Vul het blok j..j+h-1 met 1..h vooruit; knip bij maandgrens
        block_len <- min(horizon, M - j + 1L)
        # Map naar globale test-indices
        # j'th origin in maand is eerste index in idx_month_test die overeenkomt
        start_k <- j
        end_k   <- j + block_len - 1L
        # Zet in preds op de corresponderende test-rijen:
        rel <- start_k:end_k
        # fc_mean[1:block_len] correspondeert met horizons 1..block_len
        preds[idx_month_test[rel]] <- fc_mean[1:block_len]
      }
    }
  }
  
  preds
}


