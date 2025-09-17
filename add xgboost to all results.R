# --- add packages if not loaded ---
suppressPackageStartupMessages({ library(xgboost); library(Matrix) })

set.seed(42)

# --------- helper: blocked folds (contiguous, no leakage) ---------
ts_block_folds <- function(n, K = 5L) {
  sizes <- rep(floor(n / K), K); sizes[seq_len(n %% K)] <- sizes[seq_len(n %% K)] + 1L
  starts <- cumsum(c(1L, head(sizes, -1L)))
  Map(function(s, sz) seq.int(s, s + sz - 1L), starts, sizes)   # list of validation indices
}

# --------- XGBoost forecaster (train->test, CV for nrounds) ---------
run_xgboost <- function(train_dt, test_dt, fold_name) {
  t0 <- Sys.time()
  
  # features & target
  feats <- intersect(FEATURES, names(train_dt))
  stopifnot(length(feats) > 0L)
  tr <- copy(train_dt); te <- copy(test_dt)
  tr[, target := total_consumption_kWh]; te[, target := total_consumption_kWh]
  
  # recipe: one-hot + drop zero-variance (prep on train only)
  rec <- recipe(target ~ ., data = tr[, c("target", feats), with = FALSE]) %>%
    step_string2factor(all_nominal(), -all_outcomes()) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    prep(training = tr)
  
  Xtr_df <- bake(rec, tr)
  Xte_df <- bake(rec, te)
  
  y_tr <- Xtr_df$target
  Xtr_df$target <- NULL; Xte_df$target <- NULL
  
  Xtr <- as.matrix(Xtr_df)
  Xte <- as.matrix(Xte_df)
  dtrain <- xgb.DMatrix(data = Xtr, label = y_tr)
  
  # time-series CV to pick nrounds
  n <- nrow(Xtr); stopifnot(n > 100)
  folds <- ts_block_folds(n, K = 5L)
  
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = c("rmse","mae"),
    eta = 0.05,
    max_depth = 7,
    min_child_weight = 5,
    subsample = 0.9,
    colsample_bytree = 0.9,
    gamma = 0,
    lambda = 1,
    alpha = 0
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 5000,
    folds = folds,                      # contiguous validation blocks
    early_stopping_rounds = 100,
    verbose = 0
  )
  best_nrounds <- if (!is.null(cv$best_iteration)) cv$best_iteration else 500
  
  # train final model on all train
  bst <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    watchlist = list(train = dtrain),
    verbose = 0
  )
  
  preds <- predict(bst, newdata = Xte)
  
  runtime <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  list(
    results = data.table(
      fold = fold_name,
      interval = te$interval,
      actual = te$total_consumption_kWh,
      forecast = preds,
      model = "XGBOOST"
    ),
    runtime_secs = runtime,
    best_nrounds = best_nrounds,
    booster = bst
  )
}

# --------- run XGBoost ONLY over your existing FOLDS & dt ---------
stopifnot(exists("dt"), exists("FOLDS"))

xgb_res_list <- list()
xgb_rt_list  <- list()

for (nm in names(FOLDS)) {
  cat("Running XGBOOST fold:", nm, "\n")
  sp <- prep_split(dt, FOLDS[[nm]])
  out <- run_xgboost(sp$train, sp$test, nm)
  xgb_res_list[[nm]] <- out$results
  xgb_rt_list[[nm]]  <- data.table(fold = nm, model = "XGBOOST",
                                   runtime_secs = out$runtime_secs,
                                   best_nrounds = out$best_nrounds)
}

xgb_all_results  <- rbindlist(xgb_res_list, use.names = TRUE)
xgb_all_runtimes <- rbindlist(xgb_rt_list,  use.names = TRUE)

# --------- append to your existing results / runtimes ---------
if (exists("all_results")) {
  all_results <- rbindlist(list(all_results, xgb_all_results), use.names = TRUE, fill = TRUE)
} else {
  all_results <- xgb_all_results
}

if (exists("all_runtimes")) {
  all_runtimes <- rbindlist(list(all_runtimes, xgb_all_runtimes), use.names = TRUE, fill = TRUE)
} else {
  all_runtimes <- xgb_all_runtimes
}

# (optional) quick metrics just for XGBOOST
if (exists("compute_metrics")) {
  xgb_metrics <- compute_metrics(xgb_all_results)
  print(xgb_metrics[order(fold)])
}

# make sure plots later include XGBOOST nicely
all_results[, model := factor(
  model,
  levels = c("HYBRID_SARIMAX+LSTM","LSTM","XGBOOST","SARIMAX","SARIMA")
)]

