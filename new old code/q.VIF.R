library(data.table)
library(car)

target_var <- "total_consumption_kWh"

# Use hourly_data as base, ensure it's a data.table
dt <- as.data.table(model_data)

# Only keep numeric columns
raw_numeric_cols <- names(dt)[sapply(dt, is.numeric)]

# Exclude the target when building predictor set
pred_cols <- setdiff(raw_numeric_cols, target_var)

# ---- 1. Correlation filter ----
cor_vec <- sapply(pred_cols, function(v)
  cor(dt[[v]], dt[[target_var]], use = "pairwise.complete.obs")
)

cor_target_df <- data.frame(
  Variable    = names(cor_vec),
  Correlation = as.numeric(cor_vec),
  row.names   = NULL,
  check.names = FALSE
)

print(cor_target_df)

# Keep variables with |r| >= 0.1
selected_vars_corr <- cor_target_df$Variable[abs(cor_target_df$Correlation) >= 0.1]
cat("Selected based on correlation (|r| >= 0.1):",
    paste(selected_vars_corr, collapse = ", "), "\n")

# ---- 2. VIF pruning ----
vif_stepwise <- function(data, target, cand_vars, vif_cutoff = 5, max_iter = 50, verbose = TRUE) {
  cand_vars <- intersect(cand_vars, names(data))
  num_vars  <- cand_vars[sapply(data[, ..cand_vars], is.numeric)]
  num_vars  <- setdiff(num_vars, target)
  
  if (length(num_vars) < 2) stop("Need at least two numeric predictors for VIF.")
  
  # Drop zero-variance
  nzv <- sapply(num_vars, function(v) length(unique(na.omit(data[[v]]))) <= 1)
  if (any(nzv)) {
    if (verbose) message("Dropping zero-variance predictors: ", paste(num_vars[nzv], collapse = ", "))
    num_vars <- num_vars[!nzv]
  }
  
  dropped <- data.frame(step = integer(), variable = character(), VIF = numeric(), stringsAsFactors = FALSE)
  iter <- 0
  
  repeat {
    iter <- iter + 1
    if (iter > max_iter || length(num_vars) < 2) break
    
    fmla <- as.formula(paste(target, "~", paste(num_vars, collapse = " + ")))
    lm_fit <- lm(fmla, data = data)
    
    # Handle aliased predictors
    aliased <- names(coef(lm_fit))[is.na(coef(lm_fit))]
    aliased <- setdiff(aliased, "(Intercept)")
    if (length(aliased)) {
      if (verbose) message("Dropping aliased predictors: ", paste(aliased, collapse = ", "))
      num_vars <- setdiff(num_vars, aliased)
      next
    }
    
    vif_vals <- car::vif(lm_fit)
    max_vif <- max(vif_vals, na.rm = TRUE)
    if (verbose) {
      message(sprintf("Iter %d | max VIF = %.2f (%s)", iter, max_vif, names(which.max(vif_vals))[1]))
    }
    
    if (is.finite(max_vif) && max_vif > vif_cutoff) {
      worst_var <- names(which.max(vif_vals))[1]
      dropped <- rbind(dropped, data.frame(step = iter, variable = worst_var, VIF = max_vif))
      num_vars <- setdiff(num_vars, worst_var)
    } else {
      break
    }
  }
  
  final_fmla <- as.formula(paste(target, "~", paste(num_vars, collapse = " + ")))
  final_fit  <- lm(final_fmla, data = data)
  final_vif  <- data.frame(Variable = names(car::vif(final_fit)),
                           VIF = as.numeric(car::vif(final_fit)),
                           row.names = NULL)
  
  list(
    selected = num_vars,
    final_vif = final_vif,
    dropped = dropped,
    formula = final_fmla,
    model = final_fit
  )
}

res <- vif_stepwise(
  data = dt,
  target = target_var,
  cand_vars = selected_vars_corr,
  vif_cutoff = 5,
  verbose = TRUE
)

print(res$final_vif)
print(res$dropped)
cat("Final selected predictors:\n", paste(res$selected, collapse = ", "), "\n")
