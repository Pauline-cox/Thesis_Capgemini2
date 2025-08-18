# R/30_variable_selection.R
# -----------------------------------------------------------------------------
# Variable selection once on training data.
# -----------------------------------------------------------------------------

candidate_features <- c(
  # time
  "hour","hour_sin","hour_cos","weekday","is_weekend","office_hours","holiday","date",
  # lags & rolling
  "lag_24","lag_48","lag_72","lag_168","lag_336","lag_504","rollmean_24","rollmean_168",
  # building/env/occupancy
  "co2","humidity","humidity_percent","tempC","sound","lux","total_occupancy",
  # weather
  "wind_speed","sunshine_minutes","global_radiation","fog","rain","snow","thunder","ice"
)

compute_correlations <- function(dt, target, features) {
  feats_num <- features[vapply(dt[, ..features], is.numeric, logical(1))]
  if (!length(feats_num)) return(data.table(variable = character(), correlation = numeric()))
  cors <- vapply(feats_num, function(v) suppressWarnings(
    cor(dt[[v]], dt[[target]], use = "complete.obs")
  ), numeric(1))
  data.table(variable = names(cors), correlation = cors)
}

rf_importance_table <- function(dt, target, features, ntrees = 300, seed = 1234) {
  set.seed(seed)
  f <- build_formula(target, features)
  fit <- ranger::ranger(
    formula = f, data = as.data.frame(dt),
    num.trees = ntrees, importance = "impurity",
    respect.unordered.factors = TRUE
  )
  imp <- sort(fit$variable.importance, decreasing = TRUE)
  data.table(variable = names(imp), rf_importance = as.numeric(imp))
}

xgb_shap_importance <- function(dt, target, features,
                                nrounds = 120, max_depth = 6, eta = 0.1,
                                subsample = 0.8, colsample_bytree = 0.8, seed = 1234) {
  set.seed(seed)
  X <- model.matrix(build_formula(target, features), as.data.frame(dt))[, -1, drop = FALSE]
  y <- dt[[target]]
  dtrain <- xgb.DMatrix(data = X, label = y)
  params <- list(objective = "reg:squarederror",
                 max_depth = max_depth, eta = eta,
                 subsample = subsample, colsample_bytree = colsample_bytree,
                 verbosity = 0)
  xgb <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = 0)
  shap_vals <- SHAPforxgboost::shap.values(xgb_model = xgb, X_train = X)
  imp <- sort(colMeans(abs(shap_vals$shap_score), na.rm = TRUE), decreasing = TRUE)
  data.table(variable = names(imp), shap_importance = as.numeric(imp))
}

# safer: no name clash + clear fallback
select_variables_once <- function(train_dt,
                                  target = "target",
                                  features = NULL,
                                  corr_threshold = 0.15,
                                  top_k_rf = 20,
                                  top_k_shap = 20) {
  
  # Fallback: if not provided, try to use a global candidate_features
  if (is.null(features)) {
    if (exists("candidate_features", inherits = TRUE)) {
      features <- get("candidate_features", inherits = TRUE)
    } else {
      stop("`features` not provided and no global `candidate_features` found.")
    }
  }
  
  # 1) correlations
  cor_tab <- NULL
  log_step("Variable selection (Corr)", {
    cor_tab <- compute_correlations(train_dt, target, features)
  })
  
  # 2) RF importance
  rf_tab <- NULL
  log_step("Variable selection (RF importance)", {
    rf_tab <- rf_importance_table(train_dt, target, features)
    rf_tab[, rf_rank := rank(-rf_importance, ties.method = "min")]
  })
  
  # 3) XGB SHAP
  shap_tab <- NULL
  log_step("Variable selection (XGB SHAP)", {
    shap_tab <- xgb_shap_importance(train_dt, target, features)
    shap_tab[, shap_rank := rank(-shap_importance, ties.method = "min")]
  })
  
  # Merge & voting
  full <- Reduce(function(a, b) merge(a, b, by = "variable", all = TRUE),
                 list(data.table(variable = features),
                      cor_tab, rf_tab[, .(variable, rf_rank)], shap_tab[, .(variable, shap_rank)]))
  
  full[, corr_pass := as.integer(abs(coalesce(correlation, 0)) >= corr_threshold)]
  full[, rf_pass   := as.integer(!is.na(rf_rank)   & rf_rank   <= top_k_rf)]
  full[, shap_pass := as.integer(!is.na(shap_rank) & shap_rank <= top_k_shap)]
  full[, votes := corr_pass + rf_pass + shap_pass]
  full[, decision := fifelse(votes >= 3, "Keep",
                             fifelse(votes == 2, "Test", "Drop"))]
  setorder(full, -votes, variable)
  
  keep_feats <- full[decision == "Keep", variable]
  test_feats <- full[decision == "Test", variable]
  
  list(
    table = full[],
    features_for_SARIMAX = keep_feats,
    features_for_TREES   = unique(c(keep_feats, test_feats)),
    features_for_LSTM    = unique(c(keep_feats, test_feats))
  )
}

