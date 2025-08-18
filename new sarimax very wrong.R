# ========= helpers (keep with your other utils) =========

.align_matrix <- function(new_mat, ref_colnames) {
  miss <- setdiff(ref_colnames, colnames(new_mat))
  if (length(miss)) {
    new_mat <- cbind(new_mat, matrix(0, nrow(new_mat), length(miss),
                                     dimnames = list(NULL, miss)))
  }
  keep <- intersect(ref_colnames, colnames(new_mat))
  out  <- new_mat[, keep, drop = FALSE]
  out[, ref_colnames, drop = FALSE]
}

.build_xreg_recipe <- function(train, sel) {
  form <- get_formula_for_model("SARIMAX", sel)
  train_df <- as.data.frame(train)
  char_cols <- names(train_df)[vapply(train_df, is.character, logical(1))]
  for (cc in char_cols) train_df[[cc]] <- as.factor(train_df[[cc]])
  
  recipes::recipe(form, data = train_df) %>%
    recipes::step_rm(all_outcomes()) %>%
    recipes::step_dummy(all_nominal(), one_hot = TRUE) %>%
    recipes::step_zv(all_predictors()) %>%                  # zero variance
    recipes::step_nzv(all_predictors()) %>%                 # near-zero variance
    recipes::step_normalize(all_predictors()) %>%
    recipes::step_corr(all_predictors(), threshold = 0.98) %>% # high correlation
    recipes::step_lincomb(all_predictors()) %>%             # linear combos
    recipes::prep()
}

.bake_xreg <- function(rec, data) {
  df <- as.data.frame(data)
  char_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (cc in char_cols) df[[cc]] <- as.factor(df[[cc]])
  as.matrix(recipes::bake(rec, df))
}

.prune_rank <- function(X) {
  # Drop exact duplicate/collinear columns via QR rank check
  cn <- colnames(X)
  keep <- rep(TRUE, ncol(X))
  ref <- NULL
  for (j in seq_len(ncol(X))) {
    cand <- cbind(ref, X[, j, drop = FALSE])
    r1 <- qr(cand)$rank
    r0 <- if (is.null(ref)) 0 else qr(ref)$rank
    if (r1 > r0) {
      ref <- cand
    } else {
      keep[j] <- FALSE
    }
  }
  X[, keep, drop = FALSE]
}

.cap_width <- function(X, n_obs, margin = 10) {
  max_p <- max(1L, n_obs - margin)
  if (ncol(X) > max_p) {
    # keep the first max_p columns deterministically
    X <- X[, seq_len(max_p), drop = FALSE]
  }
  X
}

# rank columns by absolute correlation with y (works on already scaled x_core)
.rank_by_abs_cor <- function(X, y) {
  if (is.null(X) || ncol(X) == 0) return(integer(0))
  # guard against all-NA
  cors <- sapply(seq_len(ncol(X)), function(j) {
    suppressWarnings(cor(X[, j], y, use = "complete.obs"))
  })
  cors[is.na(cors)] <- 0
  order(abs(cors), decreasing = TRUE)
}
# --- Drop this in your utils (e.g., n.utils.R) ---

.fourier_terms <- function(t_index, periods = c(24, 168), K = c(3, 2)) {
  stopifnot(length(periods) == length(K))
  t <- as.numeric(t_index)
  cols <- list(); cn <- character(0)
  for (i in seq_along(periods)) {
    P  <- periods[i]; Ki <- K[i]
    if (Ki <= 0) next
    for (k in 1:Ki) {
      cols[[length(cols)+1]] <- sin(2*pi*k*t/P)
      cn <- c(cn, sprintf("S%d.%d", k, P))
      cols[[length(cols)+1]] <- cos(2*pi*k*t/P)
      cn <- c(cn, sprintf("C%d.%d", k, P))
    }
  }
  if (!length(cols)) return(matrix(nrow = length(t), ncol = 0))
  X <- do.call(cbind, cols)
  colnames(X) <- cn
  X
}


# ========= robust SARIMAX with Fourier and fallbacks =========

fit_sarimax <- function(train, sel, 
                        K_daily = 3, K_weekly = 2,
                        min_K_daily = 1, min_K_weekly = 1,
                        margin = 10, verbose = TRUE) {
  requireNamespace("forecast")
  y <- as.numeric(train$target)
  if (any(!is.finite(y))) stop("Target contains non-finite values.")
  n <- length(y)
  fit_sarimax <- function(train, sel, 
                          K_daily = 3, K_weekly = 2,
                          min_K_daily = 1, min_K_weekly = 1,
                          margin = 10, verbose = TRUE) {
    requireNamespace("forecast")
    y <- as.numeric(train$target)
    if (any(!is.finite(y))) stop("Target contains non-finite values.")
    n <- length(y)
    
    # Robust xreg pipeline for CORE features
    rec <- .build_xreg_recipe(train, sel)
    x_core <- .bake_xreg(rec, train)
    if (ncol(x_core)) {
      if (any(!is.finite(x_core))) stop("Non-finite values in baked xreg.")
      colnames(x_core) <- make.names(colnames(x_core), unique = TRUE)
      x_core <- .prune_rank(x_core)
    }
    
    # We'll always index time as 1...n for train; test will continue n+1...n+h
    periods <- c(24, 168)
    try_fit <- function(Kd, Kw, use_fourier = TRUE, use_core = TRUE) {
      parts <- c()
      max_p <- max(1L, n - margin)
      
      # Fourier first — NEVER trimmed
      X_four <- if (use_fourier) .fourier_terms(t_index = 1:n, periods = periods, K = c(Kd, Kw)) else NULL
      if (!is.null(X_four)) parts <- c(parts, sprintf("fourier(%d,%d)", Kd, Kw))
      
      # Core next — keep as many columns as capacity allows, ranked by |cor|
      X_core_sel <- NULL
      if (use_core && ncol(x_core)) {
        slots_left <- max(0L, max_p - ifelse(is.null(X_four), 0L, ncol(X_four)))
        if (slots_left > 0) {
          ord <- .rank_by_abs_cor(x_core, y)
          if (length(ord)) {
            keep_idx <- ord[seq_len(min(slots_left, length(ord)))]
            X_core_sel <- .prune_rank(x_core[, keep_idx, drop = FALSE])
            parts <- c("core", parts)
          }
        }
      }
      
      xreg <- if (!is.null(X_four) && !is.null(X_core_sel)) cbind(X_four, X_core_sel)
      else if (!is.null(X_four)) X_four
      else if (!is.null(X_core_sel)) X_core_sel
      else matrix(, nrow = n, ncol = 0)
      
      if (ncol(xreg)) {
        colnames(xreg) <- make.names(colnames(xreg), unique = TRUE)
        if (any(!is.finite(xreg))) stop("Non-finite values in xreg.")
      }
      
      seas <- is.null(X_four)  # if no Fourier, allow seasonal ARIMA
      fit <- try(
        forecast::auto.arima(
          y         = y,
          xreg      = if (ncol(xreg)) xreg else NULL,
          seasonal  = seas,
          stepwise  = TRUE,
          approximation = FALSE,
          allowmean = TRUE,
          allowdrift = TRUE
        ),
        silent = TRUE
      )
      list(fit = fit, xreg = xreg, desc = paste(parts, collapse = " + "), Kd = Kd, Kw = Kw)
    }
    
    # attempts ladder (same logic as before, progressively reducing K)
    attempts <- list()
    Kd <- K_daily; Kw <- K_weekly
    attempts[[length(attempts)+1]] <- try_fit(Kd, Kw, use_fourier = TRUE, use_core = TRUE)
    while ((inherits(attempts[[length(attempts)]]$fit, "try-error") ||
            inherits(attempts[[length(attempts)]]$fit, "simpleError") ||
            is.null(attempts[[length(attempts)]]$fit))) {
      if (Kd > min_K_daily) Kd <- Kd - 1 else if (Kw > min_K_weekly) Kw <- Kw - 1 else break
      attempts[[length(attempts)+1]] <- try_fit(Kd, Kw, use_fourier = TRUE, use_core = TRUE)
    }
    if (inherits(attempts[[length(attempts)]]$fit, "try-error") ||
        inherits(attempts[[length(attempts)]]$fit, "simpleError") ||
        is.null(attempts[[length(attempts)]]$fit)) {
      attempts[[length(attempts)+1]] <- try_fit(max(min_K_daily, 2), max(min_K_weekly, 1),
                                                use_fourier = TRUE, use_core = FALSE)
    }
    if (inherits(attempts[[length(attempts)]]$fit, "try-error") ||
        inherits(attempts[[length(attempts)]]$fit, "simpleError") ||
        is.null(attempts[[length(attempts)]]$fit)) {
      attempts[[length(attempts)+1]] <- try_fit(0, 0, use_fourier = FALSE, use_core = TRUE)
    }
    if (inherits(attempts[[length(attempts)]]$fit, "try-error") ||
        inherits(attempts[[length(attempts)]]$fit, "simpleError") ||
        is.null(attempts[[length(attempts)]]$fit)) {
      if (verbose) message("All SARIMAX variants failed; falling back to SARIMA seasonal=TRUE.")
      fit <- forecast::auto.arima(y, seasonal = TRUE)
      return(list(
        type = "SARIMA_FALLBACK",
        arima = fit,
        rec = NULL,
        xreg_cols = character(),
        seasonal_K = c(0,0),
        used = "SARIMA seasonal=TRUE",
        n_train = n,
        periods = periods
      ))
    }
    
    best <- attempts[[length(attempts)]]
    if (verbose) message("SARIMAX fitted with: ", best$desc,
                         " | xreg cols: ", ncol(best$xreg),
                         " | seasonal=", ifelse(grepl("fourier", best$desc), "FALSE", "TRUE"))
    
    list(
      type        = "SARIMAX_FOURIER",
      arima       = best$fit,
      rec         = rec,
      xreg_cols   = colnames(best$xreg),
      seasonal_K  = c(best$Kd, best$Kw),
      n_train     = n,                # <-- store train length
      periods     = periods,          # <-- store periods (24, 168)
      used        = best$desc
    )
  }
  

  .build_sarimax_xreg_test <- function(model, train, test) {
    if (is.null(model$rec)) return(matrix(, nrow = nrow(test), ncol = 0))
    
    # Core xreg via stored recipe
    x_core_test <- .bake_xreg(model$rec, test)
    if (ncol(x_core_test)) colnames(x_core_test) <- make.names(colnames(x_core_test), unique = TRUE)
    
    # Manual Fourier for future indices: n_train+1 ... n_train+h
    use_fourier <- all(model$seasonal_K > 0)
    X_four_test <- NULL
    if (use_fourier) {
      t_test <- (model$n_train + 1):(model$n_train + nrow(test))
      X_four_test <- .fourier_terms(t_index = t_test, periods = model$periods, K = model$seasonal_K)
    }
    
    # Combine (Fourier FIRST)
    x_test <- if (!is.null(X_four_test) && ncol(x_core_test)) cbind(X_four_test, x_core_test)
    else if (!is.null(X_four_test)) X_four_test
    else x_core_test
    
    # Align to training columns (adds missing=0, drops extras, reorders)
    if (!is.null(model$xreg_cols) && length(model$xreg_cols)) {
      x_test <- .align_matrix(x_test, model$xreg_cols)
    } else if (ncol(x_test) == 0) {
      x_test <- NULL
    }
    x_test
  }
  
  predict_sarimax <- function(model, train, test) {
    x_test <- .build_sarimax_xreg_test(model, train, test)
    fc <- forecast::forecast(model$arima, xreg = x_test)
    as.numeric(fc$mean)
  }
  
log_step("Fit SARIMAX (robust)", {
  sarimax_model <- fit_sarimax(train_data, sel, K_daily = 5, K_weekly = 3, verbose = TRUE)
})
pred_sarimax <- predict_sarimax(sarimax_model, train_data, test_data)



models <- list(
  SARIMAX      = pred_sarimax

)

library(Metrics)   # for mae, rmse, mape
library(ggplot2)

# --- Metrics ---
actual <- test_data$target
pred   <- pred_sarimax

mae_val  <- mae(actual, pred)
rmse_val <- rmse(actual, pred)
mape_val <- mape(actual, pred) * 100  # %
ss_res   <- sum((actual - pred)^2, na.rm = TRUE)
ss_tot   <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
r2_val   <- 1 - ss_res/ss_tot

cat("\n==== SARIMAX Evaluation ====\n")
cat(sprintf("MAE  : %.3f\n", mae_val))
cat(sprintf("RMSE : %.3f\n", rmse_val))
cat(sprintf("MAPE : %.2f%%\n", mape_val))
cat(sprintf("R²   : %.3f\n", r2_val))

# --- Plot ---
eval_df <- data.frame(
  Index = seq_along(actual),
  Actual = actual,
  Predicted = pred
)

ggplot(eval_df, aes(x = Index)) +
  geom_line(aes(y = Actual, colour = "Actual"), size = 0.8) +
  geom_line(aes(y = Predicted, colour = "Predicted"), size = 0.8, linetype = "dashed") +
  labs(title = "SARIMAX: Actual vs Predicted",
       subtitle = sprintf("MAE=%.2f | RMSE=%.2f | MAPE=%.2f%% | R²=%.3f",
                          mae_val, rmse_val, mape_val, r2_val),
       y = "Target", colour = "Legend") +
  theme_minimal()
