library(data.table)
library(lubridate)
library(keras)
library(recipes)

# ---- config (you can tweak) ----
timesteps    <- 168
epochs       <- 80
batch_size   <- 32
patience     <- 6
units1       <- 128
units2       <- 64
dropout_rate <- 0.2
final_activation <- "linear"

# your chosen predictors
xreg_num <- c("lag_24","lag_168","rollmean_24","hour_sin","hour_cos",
              "total_occupancy","co2","global_radiation","tempC")
xreg_cat <- c("weekday")

set.seed(42); try({ tensorflow::tf$random$set_seed(42) }, silent = TRUE)

# vectorized sequence builder for a contiguous baked slice
make_seq_block <- function(baked, timesteps) {
  X <- as.matrix(baked[, setdiff(names(baked), "target"), drop = FALSE])
  y <- baked$target
  n <- nrow(baked); s <- n - timesteps
  if (s <= 0) return(list(X = array(0, dim = c(0, timesteps, ncol(X))), y = numeric(0)))
  idx <- matrix(rep(seq_len(timesteps), each = s) + rep(0:(s-1), times = timesteps), nrow = s)
  X3  <- array(X[idx, , drop = FALSE], dim = c(s, timesteps, ncol(X)))
  yv  <- y[timesteps + seq_len(s)]
  list(X = X3, y = yv)
}

build_lstm <- function(input_shape, units1=128, units2=64, dropout=0.2, lr=1e-3, act="linear"){
  keras_model_sequential() |>
    layer_lstm(units = units1, return_sequences = TRUE, input_shape = input_shape) |>
    layer_dropout(dropout) |>
    layer_lstm(units = units2) |>
    layer_dropout(dropout) |>
    layer_dense(units = 1, activation = act) |>
    compile(optimizer = optimizer_adam(lr), loss = "mse", metrics = "mae")
}

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
  stopifnot(all(c("interval","target") %in% names(all_dt)))
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
    
    # contiguous block for the month windows
    all_idx   <- match(ts_m, all_dt$interval)
    start_row <- min(all_idx) - timesteps
    end_row   <- max(all_idx) - 1
    if (start_row < 1) next
    slice_rows <- all_dt[start_row:end_row]
    
    # keep only target + chosen predictors
    keep_cols <- unique(c("target", xreg_num, xreg_cat))
    tr_df <- as.data.frame(tr_rows[, ..keep_cols])
    sl_df <- as.data.frame(slice_rows[, ..keep_cols])
    
    # recipe on train only (dummy weekday, scale numerics)
    rec <- recipe(tr_df) |>
      update_role(target, new_role = "outcome") |>
      update_role(all_of(setdiff(keep_cols, "target")), new_role = "predictor") |>
      step_dummy(all_of(xreg_cat), one_hot = TRUE) |>
      step_range(all_numeric(), -all_outcomes(), min = 0, max = 1) |>
      prep()
    
    baked_tr <- bake(rec, tr_df)
    baked_sl <- bake(rec, sl_df)
    
    # target min-max from TRAIN ONLY for inverse transform
    tmin <- min(tr_df$target, na.rm = TRUE)
    tmax <- max(tr_df$target, na.rm = TRUE)
    rng  <- if (is.finite(tmax - tmin) && (tmax - tmin) > 1e-8) (tmax - tmin) else 1
    baked_tr$target <- (tr_df$target - tmin) / rng
    baked_sl$target <- (sl_df$target - tmin) / rng
    
    # sequences from TRAIN
    tr <- make_seq_block(baked_tr, timesteps)
    if (length(tr$y) < 64) next
    
    # train/val split
    n <- dim(tr$X)[1]; n_val <- max(1, floor(0.2*n)); n_tr <- n - n_val
    X_tr <- tr$X[1:n_tr, , , drop = FALSE];  y_tr <- tr$y[1:n_tr]
    X_va <- tr$X[(n_tr+1):n, , , drop = FALSE]; y_va <- tr$y[(n_tr+1):n]
    
    model <- build_lstm(c(timesteps, dim(tr$X)[3]),
                        units1 = units1, units2 = units2,
                        dropout = dropout, lr = 1e-3, act = final_activation)
    cb <- list(callback_early_stopping(monitor="val_loss", patience=patience, restore_best_weights=TRUE))
    invisible(model |> fit(X_tr, y_tr, validation_data = list(X_va, y_va),
                           epochs = epochs, batch_size = batch_size, verbose = 1, callbacks = cb))
    
    # sequences for the month (one block)
    sl <- make_seq_block(baked_sl, timesteps)
    if (length(sl$y) == 0) next
    yh_norm <- as.numeric(model |> predict(sl$X, verbose = 0))
    yh      <- yh_norm * rng + tmin
    
    # map to test rows in this month
    end_times <- slice_rows$interval[(timesteps+1):nrow(slice_rows)]
    mtch <- match(ts_m, end_times)
    ok   <- which(!is.na(mtch))
    preds[idxm[ok]] <- yh[mtch[ok]]
  }
  
  preds
}

t0 <- Sys.time()
pred_lstm <- lstm_monthly_retrain_xreg(
  train_dt = train_data,
  test_dt  = test_data,
  horizon  = horizon,
  timesteps = timesteps,
  xreg_num = xreg_num,
  xreg_cat = xreg_cat,
  units1 = units1, units2 = units2,
  dropout = dropout_rate, epochs = epochs,
  batch_size = batch_size, patience = patience,
  final_activation = "linear"
)
t1 <- Sys.time()
runtime_lstm <- as.numeric(difftime(t1, t0, units = "secs"))

metrics_lstm <- eval_metrics(actual, pred_lstm)
metrics_lstm$runtime_seconds <- runtime_lstm
print(metrics_lstm)

plot_dt_lstm <- data.table(
  target_time = test_data$interval + hours(horizon),
  actual      = actual,
  pred        = pred_lstm
)
gg_lstm <- ggplot(plot_dt_lstm, aes(x = target_time)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 0.7) +
  geom_line(aes(y = pred,   color = "LSTM"),   linewidth = 0.7, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "#1f77b4", "LSTM" = "#d62728")) +
  labs(
    title = sprintf("LSTM (monthly retrain, recipes) — h = %d, lookback = %d", horizon, seq_timesteps),
    x = "Time (target timestamp)", y = "Energy consumption (kWh)", color = "Series",
    caption = sprintf("MAE=%.2f | RMSE=%.2f | MAPE=%.2f%% | R²=%.3f | Run=%.1fs",
                      metrics_lstm$MAE, metrics_lstm$RMSE, metrics_lstm$MAPE, metrics_lstm$R2, runtime_lstm)
  ) +
  theme_minimal()
print(gg_lstm)


plot_dt_lstm <- data.table(
  target_time = test_data$interval + hours(horizon),
  actual      = actual,
  pred        = pred_lstm
)
gg_lstm <- ggplot(plot_dt_lstm, aes(x = target_time)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 0.7) +
  geom_line(aes(y = pred,   color = "LSTM"),   linewidth = 0.7, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "#1f77b4", "LSTM" = "#d62728")) +
  labs(
    title = sprintf("LSTM (monthly retrain, recipes) — h = %d, lookback = %d",
                    horizon, timesteps),
    x = "Time (target timestamp)", 
    y = "Energy consumption (kWh)", 
    color = "Series",
    caption = sprintf("MAE=%.2f | RMSE=%.2f | MAPE=%.2f%% | R²=%.3f | Run=%.1fs",
                      metrics_lstm$MAE, metrics_lstm$RMSE, metrics_lstm$MAPE, 
                      metrics_lstm$R2, runtime_lstm)
  ) +
  theme_minimal()
print(gg_lstm)


