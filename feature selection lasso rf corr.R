# ============================================
# Feature Selection for SARIMAX & LSTM
# ============================================
library(data.table)
library(car)
library(glmnet)
library(randomForest)
library(caret)

target_var <- "total_consumption_kWh"

# ---------------------------
# 1) SARIMAX Feature Selection
# ---------------------------
sarimax_fs <- function(dt, target_var, cor_cutoff = 0.3, vif_cutoff = 5) {
  dt <- copy(dt)
  
  # Drop categorical/time features (SARIMAX handles seasonality separately)
  drop_feats <- c("interval", "date", "year", "month", "weekday", "holiday")
  dt[, (intersect(drop_feats, names(dt))) := NULL]
  
  # Keep only numeric cols
  num_cols <- names(which(sapply(dt, is.numeric)))
  num_dt   <- dt[, ..num_cols]
  
  # Correlation with target
  pred_cols <- setdiff(num_cols, target_var)
  cor_vec <- sapply(pred_cols, function(v)
    cor(num_dt[[v]], num_dt[[target_var]], use = "pairwise.complete.obs", method = "spearman")
  )
  
  cor_target_df <- data.frame(
    Variable    = names(cor_vec),
    Correlation = as.numeric(cor_vec),
    row.names   = NULL
  )
  
  # Filter by correlation
  selected_vars_corr <- cor_target_df$Variable[abs(cor_target_df$Correlation) >= cor_cutoff]
  
  # Stepwise VIF pruning
  vif_stepwise <- function(data, target, cand_vars, vif_cutoff = 5, max_iter = 50) {
    cand_vars <- intersect(cand_vars, names(data))
    num_vars  <- setdiff(cand_vars, target)
    if (length(num_vars) < 2) return(list(selected = num_vars, vif = NULL))
    
    iter <- 0
    dropped <- c()
    repeat {
      iter <- iter + 1
      if (iter > max_iter || length(num_vars) < 2) break
      fmla <- as.formula(paste(target, "~", paste(num_vars, collapse = " + ")))
      lm_fit <- lm(fmla, data = data)
      vif_vals <- car::vif(lm_fit)
      if (max(vif_vals, na.rm = TRUE) > vif_cutoff) {
        worst <- names(which.max(vif_vals))[1]
        dropped <- c(dropped, worst)
        num_vars <- setdiff(num_vars, worst)
      } else break
    }
    # Final VIF table
    fmla <- as.formula(paste(target, "~", paste(num_vars, collapse = " + ")))
    lm_fit <- lm(fmla, data = data)
    vif_final <- car::vif(lm_fit)
    vif_df <- data.frame(Variable = names(vif_final), VIF = as.numeric(vif_final))
    
    list(selected = num_vars, dropped = dropped, vif = vif_df)
  }
  
  res <- vif_stepwise(dt, target_var, selected_vars_corr, vif_cutoff)
  
  list(
    correlation = cor_target_df,
    selected = res$selected,
    dropped = res$dropped,
    vif = res$vif
  )
}
sarimax_vars <- sarimax_fs(processed_data, target_var)

sarimax_vars <- sarimax_fs(model_data, target_var)
cat("\nSARIMAX Selected Variables:\n", paste(sarimax_vars$selected, collapse = ", "), "\n")


# ---------------------------
# 2) LSTM Feature Selection
# ---------------------------
lstm_fs <- function(dt, target_var) {
  dt <- copy(dt)
  
  # Keep numeric predictors
  num_cols <- names(which(sapply(dt, is.numeric)))
  pred_cols <- setdiff(num_cols, target_var)
  X <- as.matrix(dt[, ..pred_cols])
  y <- dt[[target_var]]
  
  # Random Forest importance
  rf_model <- randomForest::randomForest(x = X, y = y, importance = TRUE, ntree = 200)
  rf_imp_obj <- randomForest::importance(rf_model)
  if (is.matrix(rf_imp_obj) && "%IncMSE" %in% colnames(rf_imp_obj)) {
    rf_imp_obj <- rf_imp_obj[, "%IncMSE"]
  } else if (is.matrix(rf_imp_obj)) {
    rf_imp_obj <- rf_imp_obj[, 1]
  }
  rf_ranked <- names(sort(rf_imp_obj, decreasing = TRUE))
  
  # LASSO selection
  cvfit <- glmnet::cv.glmnet(X, y, alpha = 1, nfolds = 5)
  coef_lasso <- coef(cvfit, s = "lambda.min")
  lasso_vars <- rownames(coef_lasso)[which(coef_lasso != 0)]
  lasso_vars <- setdiff(lasso_vars, "(Intercept)")
  
  list(
    rf_imp_obj = rf_imp_obj,
    rf_ranked = rf_ranked,
    lasso_selected = lasso_vars,
    X = scale(X),
    y = y
  )
}

lstm_vars <- lstm_fs(model_data, target_var)


# ---------------------------
# 3) Unified Summary Table
# ---------------------------
make_fs_table <- function(rf_imp_obj, lasso_vars, sarimax_vars, cor_df, vif_df, top_n = 35) {
  rf_imp_sorted <- sort(rf_imp_obj, decreasing = TRUE)
  rf_top <- head(rf_imp_sorted, top_n)
  
  fs_table <- data.frame(
    Variable = names(rf_top),
    RF_Importance = as.numeric(rf_top),
    Selected_by_LASSO = names(rf_top) %in% lasso_vars,
    Selected_by_SARIMAX = names(rf_top) %in% sarimax_vars,
    Correlation = cor_df$Correlation[match(names(rf_top), cor_df$Variable)],
    VIF = vif_df$VIF[match(names(rf_top), vif_df$Variable)],
    stringsAsFactors = FALSE
  )
  
  return(fs_table)
}

fs_table <- make_fs_table(
  lstm_vars$rf_imp_obj,
  lstm_vars$lasso_selected,
  sarimax_vars$selected,
  sarimax_vars$correlation,
  sarimax_vars$vif,
  top_n = 30
)

cat("\nTable: Feature Selection Outcomes Across Methods\n")
print(fs_table)

