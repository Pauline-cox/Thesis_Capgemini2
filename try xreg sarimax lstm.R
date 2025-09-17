# ============================================
# Add: LSTM (temporal/external) + SARIMAX (temporal/external)
# Append results into existing all_res[[fold]]
# ============================================

# --- feature / xreg sets ---
FEATS_TEMPORAL <- c("holiday","office_hours","is_weekend",
                    "hour_sin","hour_cos","weekday",
                    "lag_24","lag_72","lag_168","lag_336","lag_504",
                    "rollmean_24","rollmean_168")

FEATS_EXTERNAL <- c("total_occupancy","co2","tempC","humidity","sound","lux",
                    "temperature","global_radiation")

XREG_TEMPORAL  <- c("lag_24","lag_168", "holiday")
XREG_EXTERNAL  <- c("total_occupancy","co2","lux","global_radiation")

# --- LSTM variant that takes an explicit feature set ---
run_lstm_with_feats <- function(train_dt, test_dt, feats, fold_name, model_label) {
  t0 <- Sys.time()
  train_dt <- copy(train_dt); test_dt <- copy(test_dt)
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
  
  # scale target on train only
  y_min <- min(y_train, na.rm = TRUE); y_max <- max(y_train, na.rm = TRUE)
  scale_y  <- function(y) (y - y_min) / (y_max - y_min + 1e-6)
  invert_y <- function(y) y * (y_max - y_min) + y_min
  y_train_s <- scale_y(y_train)
  
  # sequences
  make_seq <- function(X, y, lookback = LOOKBACK, horizon = HORIZON) {
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
  tr_seq <- make_seq(X_train, y_train_s)
  
  # same tuned model as your main LSTM
  units1 <- 70L; units2 <- 63L; dropout <- 0.231; lr <- 0.001950638; batch_sz <- 32L
  model <- keras_model_sequential() %>%
    layer_lstm(units = units1, input_shape = c(LOOKBACK, ncol(X_train)), return_sequences = TRUE) %>%
    layer_dropout(rate = dropout) %>%
    layer_lstm(units = units2, return_sequences = FALSE) %>%
    layer_dense(units = HORIZON, activation = "sigmoid")
  model %>% compile(optimizer = optimizer_nadam(learning_rate = lr), loss = "mse")
  
  model %>% fit(tr_seq$X, tr_seq$y,
                epochs = 120, batch_size = batch_sz, validation_split = 0.2,
                verbose = 0,
                callbacks = list(
                  callback_early_stopping(patience = 10, restore_best_weights = TRUE),
                  callback_reduce_lr_on_plateau(factor = 0.5, patience = 5)
                ))
  
  # rolling 24h across test
  n_test <- nrow(X_test)
  fc <- numeric(0); act <- numeric(0); ts_out <- as.POSIXct(character(0))
  for (i in seq(1, n_test, by = HORIZON)) {
    block_end <- min(i + HORIZON - 1L, n_test)
    need_from_test  <- min(LOOKBACK, i - 1L)
    need_from_train <- LOOKBACK - need_from_test
    hist_mat <- if (need_from_train > 0) {
      rbind(safe_tail(X_train, need_from_train),
            if (need_from_test > 0) X_test[(i - need_from_test):(i - 1L), , drop = FALSE] else NULL)
    } else {
      X_test[(i - need_from_test):(i - 1L), , drop = FALSE]
    }
    Xin   <- array(hist_mat, dim = c(1, LOOKBACK, ncol(X_test)))
    predS <- predict(model, Xin, verbose = 0)
    preds <- invert_y(as.numeric(predS))[1:(block_end - i + 1L)]
    fc  <- c(fc, preds)
    act <- c(act, y_test[i:block_end])
    ts_out <- c(ts_out, test_dt$interval[i:block_end])
  }
  runtime <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  
  list(results = data.table(fold = fold_name, interval = ts_out,
                            actual = act, forecast = fc, model = model_label),
       runtime_secs = runtime)
}

# --- run & append into all_res ---
for (nm in names(FOLDS)) {
  cat("Running fold:", nm, "\n")
  
  sp <- prep_split(dt, FOLDS[[nm]]); tr <- sp$train; te <- sp$test
  
  # LSTM variants
  lstm_tem <- run_lstm_with_feats(tr, te, FEATS_TEMPORAL, nm, "LSTM_temporal")$results
  lstm_ext <- run_lstm_with_feats(tr, te, FEATS_EXTERNAL, nm, "LSTM_external")$results
  
  # SARIMAX variants (re-use your run_sarimax())
  sarx_tem <- run_sarimax(tr, te, XREG_TEMPORAL, nm)$results[, model := "SARIMAX_temporal"]
  sarx_ext <- run_sarimax(tr, te, XREG_EXTERNAL, nm)$results[, model := "SARIMAX_external"]
  
  # append to the fold table
  all_res[[nm]] <- rbindlist(
    list(all_res[[nm]], lstm_tem, lstm_ext, sarx_tem, sarx_ext),
    use.names = TRUE, fill = TRUE
  )
}

# Rebuild the combined table if you use it later
all_results <- rbindlist(all_res, use.names = TRUE, fill = TRUE)
