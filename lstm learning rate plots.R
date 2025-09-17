run_fold_lstm <- function(dt, fold_name, fold_def) {
  sp <- prep_split(dt, fold_def)
  tr <- sp$train; te <- sp$test
  
  lstm_out <- run_lstm(tr, te, fold_name)
  
  # print only (no ggsave)
  p <- ggplot(lstm_out$results, aes(x = interval)) +
    geom_line(aes(y = actual), linewidth = 0.45, color = "black") +
    geom_line(aes(y = forecast), linewidth = 0.45, color = "#66a61e") +
    labs(title = sprintf("Forecast vs Actual — LSTM (%s)", fold_name),
         x = "Time", y = "kWh") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
  print(p)
  
  list(
    results  = lstm_out$results,
    runtimes = data.table(fold = fold_name, model = "LSTM", runtime_secs = lstm_out$runtime_secs),
    history  = lstm_out$history
  )
}

run_lstm <- function(train_dt, test_dt, fold_name) {
  t0 <- Sys.time()
  setorder(train_dt, interval); setorder(test_dt, interval)
  
  # ---- 1) Target + feature set ----
  feats <- intersect(FEATURES, names(train_dt))
  train_dt[, target := total_consumption_kWh]
  test_dt[,  target := total_consumption_kWh]
  
  # Drop raw time objects early (they break Keras inputs)
  drop_cols <- intersect(c("interval","date"), feats)
  feats_use <- setdiff(feats, drop_cols)
  
  # ---- 2) Recipe: encode categoricals, impute, scale numerics ----
  # ---- 2) Sanitize + Recipe: encode categoricals, impute, scale numerics ----
  all_data <- rbind(copy(train_dt), copy(test_dt), fill = TRUE)
  
  # force time-ish categoricals to simple atomic vectors (no POSIXlt/list)
  cat_cols <- intersect(c("weekday","month","year"), feats_use)
  for (cn in cat_cols) {
    if (is.list(all_data[[cn]]) || inherits(all_data[[cn]], "POSIXlt")) {
      all_data[, (cn) := as.character(as.POSIXct(get(cn)))]  # make a plain character
    } else if (!is.character(all_data[[cn]]) && !is.factor(all_data[[cn]])) {
      all_data[, (cn) := as.character(get(cn))]              # safe default
    }
  }
  if ("hour" %in% feats_use) {
    # ensure hour is numeric scalar, not POSIXlt sublist
    if (is.list(all_data$hour) || inherits(all_data$hour, "POSIXlt")) {
      all_data[, hour := as.integer(hour(as.POSIXct(interval, tz = "UTC")))]
    } else {
      all_data[, hour := as.integer(hour)]
    }
  }
  
  rec <- recipe(target ~ ., data = all_data[, c("target", feats_use), with = FALSE]) %>%
    step_string2factor(all_nominal(), -all_outcomes()) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    step_impute_mean(all_numeric(), -all_outcomes()) %>%   # avoid NAs
    step_range(all_numeric(), -all_outcomes()) %>%         # [0,1] scaling
    prep(training = train_dt)
  
  # after prep(...)
  norm_train <- as.data.table(bake(rec, train_dt))
  norm_test  <- as.data.table(bake(rec, test_dt))
  
  
  # ---- 3) GUARANTEE numeric-only predictor matrix ----
  # convert logicals (if any) to numeric 0/1
  logi_tr <- names(Filter(is.logical, norm_train))
  logi_te <- names(Filter(is.logical, norm_test))
  if (length(logi_tr)) norm_train[, (logi_tr) := lapply(.SD, as.numeric), .SDcols = logi_tr]
  if (length(logi_te)) norm_test[,  (logi_te) := lapply(.SD, as.numeric), .SDcols = logi_te]
  
  # keep only numeric predictors present in both sets
  is_num_tr <- vapply(norm_train, is.numeric, logical(1))
  is_num_te <- vapply(norm_test,  is.numeric, logical(1))
  predictors <- setdiff(intersect(names(norm_train)[is_num_tr], names(norm_test)[is_num_te]), "target")
  stopifnot(length(predictors) > 0L)
  
  # now this is valid (data.table syntax)
  X_train <- as.matrix(norm_train[, ..predictors])
  X_test  <- as.matrix(norm_test[,  ..predictors])
  
  y_train <- train_dt$target
  y_test  <- test_dt$target
  
  # ---- 4) Scale target on TRAIN only ----
  y_min <- min(y_train, na.rm = TRUE); y_max <- max(y_train, na.rm = TRUE)
  scale_y  <- function(y) (y - y_min) / (y_max - y_min + 1e-6)
  invert_y <- function(y) y * (y_max - y_min) + y_min
  y_train_s <- scale_y(y_train)
  
  # ---- 5) Build supervised sequences ----
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
  
  # ---- 6) LSTM model ----
  units1 <- 70L; units2 <- 63L; dropout <- 0.231; lr <- 0.001950638; batch_sz <- 32L
  
  model <- keras_model_sequential() %>%
    layer_lstm(units = units1, input_shape = c(LOOKBACK, ncol(X_train)), return_sequences = TRUE) %>%
    layer_dropout(rate = dropout) %>%
    layer_lstm(units = units2, return_sequences = FALSE) %>%
    layer_dense(units = HORIZON, activation = "sigmoid")
  
  model %>% compile(optimizer = optimizer_nadam(learning_rate = lr),
                    loss = "mse", metrics = "mae")
  
  # log LR each epoch for plotting
  cb_log_lr <- callback_lambda(
    on_epoch_end = function(epoch, logs) {
      logs$lr <- as.numeric(k_get_value(model$optimizer$lr))
    }
  )
  
  history <- model %>% fit(
    x = train_seq$X, y = train_seq$y,
    epochs = 120, batch_size = batch_sz, validation_split = 0.2,
    verbose = 1,
    callbacks = list(
      callback_early_stopping(patience = 10, restore_best_weights = TRUE),
      callback_reduce_lr_on_plateau(factor = 0.5, patience = 5),
      cb_log_lr
    )
  )
  
  # ---- 7) Forecast entire TEST in 24h blocks (seed with history) ----
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


# ===== MAIN (LSTM only) =====
stopifnot(exists("model_data"))
dt <- as.data.table(model_data)

all_res <- list(); all_rt <- list(); all_hist <- list()
for (nm in names(FOLDS)) {
  cat("Running LSTM fold:", nm, "\n")
  out <- run_fold_lstm(dt, nm, FOLDS[[nm]])
  all_res[[nm]]  <- out$results
  all_rt[[nm]]   <- out$runtimes
  all_hist[[nm]] <- out$history
}
all_results  <- rbindlist(all_res, use.names = TRUE)
all_runtimes <- rbindlist(all_rt,  use.names = TRUE)

metrics <- compute_metrics(all_results)
metrics <- merge(metrics, all_runtimes, by = c("fold","model"), all.x = TRUE)
metrics[, runtime_minutes := runtime_secs/60]
print(metrics[order(fold)])
suppressPackageStartupMessages({
  library(data.table); library(ggplot2)
})

# --- Tidy Keras histories into one table ---
tidy_history <- function(history, fold) {
  mets <- history$metrics
  n    <- length(mets[[1]])
  dt   <- data.table(epoch = seq_len(n))
  for (nm in names(mets)) dt[, (nm) := as.numeric(mets[[nm]])]
  keep <- intersect(c("loss","val_loss","lr"), names(dt))
  melt(dt[, c("epoch", keep), with = FALSE][, fold := fold],
       id.vars = c("epoch","fold"),
       variable.name = "metric", value.name = "value")[]
}

# If you already have all_hist (named list of histories) this will use it.
# If not, but you have a single `history` in memory, this creates a minimal list.
if (!exists("all_hist") && exists("history")) {
  all_hist <- list(FOLD = history)
}

stopifnot(exists("all_hist"))
hist_dt <- rbindlist(lapply(names(all_hist),
                            function(f) tidy_history(all_hist[[f]], f)), fill = TRUE)

train_col <- "black"
val_col   <- "steelblue"
line_size <- 0.7

# --- MSE (loss): faceted by fold, dashed validation, dotted best-val epoch ---
loss_dt <- hist_dt[metric %in% c("loss","val_loss")]
loss_dt[, set := ifelse(metric == "val_loss", "Validation", "Train")]
best_val <- loss_dt[set == "Validation", .SD[which.min(value)], by = fold]

p_mse_facet <- ggplot(loss_dt, aes(epoch, value, color = set, linetype = set)) +
  geom_line(linewidth = line_size) +
  geom_vline(data = best_val, aes(xintercept = epoch),
             linetype = "dotted", linewidth = 0.35, color = "grey40", inherit.aes = FALSE) +
  geom_point(data = best_val, aes(epoch, value), color = val_col, size = 1.2, inherit.aes = FALSE) +
  scale_color_manual(values = c(Train = train_col, Validation = val_col)) +
  scale_linetype_manual(values = c(Train = "solid", Validation = "dashed")) +
  facet_wrap(~ fold, ncol = 1, scales = "free_y") +
  labs(title = "LSTM Training — MSE over epochs", x = "Epoch", y = "MSE", color = NULL, linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"))

print(p_mse_facet)

# --- MSE (loss): overlayed folds (solid=train, dashed=val) ---
p_mse_overlay <- ggplot(loss_dt, aes(epoch, value, color = fold, linetype = set)) +
  geom_line(linewidth = line_size) +
  scale_linetype_manual(values = c(Train = "solid", Validation = "dashed")) +
  labs(title = "LSTM Training — MSE over epochs",
       x = "Epoch", y = "MSE", color = "Fold", linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"))

print(p_mse_overlay)

# --- Learning rate (if logged via callback): facet + overlay ---
if ("lr" %in% hist_dt$metric) {
  lr_dt <- hist_dt[metric == "lr"]
  
  p_lr_facet <- ggplot(lr_dt, aes(epoch, value)) +
    geom_line(linewidth = line_size, color = train_col) +
    facet_wrap(~ fold, ncol = 1, scales = "free_y") +
    labs(title = "Learning rate over epochs", x = "Epoch", y = "LR") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
  print(p_lr_facet)
  
  p_lr_overlay <- ggplot(lr_dt, aes(epoch, value, color = fold)) +
    geom_line(linewidth = line_size) +
    labs(title = "Learning rate over epochs", x = "Epoch", y = "LR", color = "Fold") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  print(p_lr_overlay)
} else {
  message("No `lr` recorded; add an LR logging callback in run_lstm() if you want LR plots.")
}

