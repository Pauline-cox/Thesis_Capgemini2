library(data.table)

target_var <- "total_consumption_kWh"

# Start from your raw numeric columns and drop the target if it's in there
x_dt <- as.data.table(hourly_data)[, ..raw_numeric_cols]
pred_cols <- setdiff(names(x_dt), target_var)

# Compute correlations column-wise to force names
cor_vec <- sapply(pred_cols, function(v)
  cor(x_dt[[v]], hourly_data[[target_var]], use = "pairwise.complete.obs")
)

# Build a tidy data frame
cor_target_df <- data.frame(
  Variable    = names(cor_vec),
  Correlation = as.numeric(cor_vec),
  row.names   = NULL,
  check.names = FALSE
)

print(cor_target_df)

# Keep variables with |r| >= 0.1 (adjust threshold as you like)
selected_vars_corr <- cor_target_df$Variable[abs(cor_target_df$Correlation) >= 0.1]
cat("Selected based on correlation (|r| >= 0.1):",
    paste(selected_vars_corr, collapse = ", "), "\n")

vif_stepwise <- function(data, target, cand_vars, vif_cutoff = 5, max_iter = 50, verbose = TRUE) {
  # Keep only numeric predictors and remove the target if present
  cand_vars <- intersect(cand_vars, names(data))
  num_vars  <- cand_vars[sapply(data[, ..cand_vars], is.numeric)]
  num_vars  <- setdiff(num_vars, target)
  
  if (length(num_vars) < 2) {
    stop("Need at least two numeric predictors to compute VIF.")
  }
  
  # Drop constant / near-constant columns (zero variance)
  nzv <- sapply(num_vars, function(v) length(unique(na.omit(data[[v]]))) <= 1)
  if (any(nzv)) {
    if (verbose) message("Dropping zero-variance predictors: ", paste(num_vars[nzv], collapse = ", "))
    num_vars <- num_vars[!nzv]
  }
  
  dropped <- data.frame(step = integer(), variable = character(), VIF = numeric(), stringsAsFactors = FALSE)
  iter <- 0
  
  repeat {
    iter <- iter + 1
    if (iter > max_iter) {
      warning("Reached max_iter without satisfying VIF cutoff.")
      break
    }
    if (length(num_vars) < 2) break
    
    # Build formula and fit
    fmla <- as.formula(paste(target, "~", paste(num_vars, collapse = " + ")))
    lm_fit <- lm(fmla, data = data)
    
    # Drop aliased predictors if any (singular fit)
    aliased <- names(coef(lm_fit))[is.na(coef(lm_fit))]
    aliased <- setdiff(aliased, "(Intercept)")
    if (length(aliased)) {
      if (verbose) message("Dropping aliased predictors (singular fit): ", paste(aliased, collapse = ", "))
      num_vars <- setdiff(num_vars, aliased)
      next
    }
    
    # Compute VIF
    vif_vals <- car::vif(lm_fit)
    vif_vec  <- as.numeric(vif_vals)
    names(vif_vec) <- names(vif_vals)
    
    max_vif <- max(vif_vec, na.rm = TRUE)
    if (verbose) {
      message(sprintf("Iter %d | max VIF = %.2f (%s)", iter, max_vif, names(which.max(vif_vec))[1]))
    }
    
    if (is.finite(max_vif) && max_vif > vif_cutoff) {
      worst_var <- names(which.max(vif_vec))[1]
      dropped <- rbind(dropped, data.frame(step = iter, variable = worst_var, VIF = max_vif))
      num_vars <- setdiff(num_vars, worst_var)
    } else {
      break
    }
  }
  
  # Final fit + VIF table
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
  data = hourly_data,
  target = target_var,
  cand_vars = selected_vars_corr,
  vif_cutoff = 5,
  verbose = TRUE
)

print(res$final_vif)
print(res$dropped)
cat("Final selected predictors:\n", paste(res$selected, collapse = ", "), "\n")


