# R/10_utils.R
# -----------------------------------------------------------------------------
# Logging, formula helpers, evaluation metrics, plotting helpers.
# -----------------------------------------------------------------------------

log_step <- function(title, expr) {
  cat(sprintf("\n---- %s: START (%s) ----\n", title, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  t0 <- Sys.time()
  on.exit({
    t1 <- Sys.time()
    cat(sprintf("---- %s: END   (%.2f sec) ----\n", title, as.numeric(difftime(t1, t0, units = "secs"))))
  })
  force(expr)
}

build_formula <- function(target, features) {
  as.formula(paste(target, "~", paste(features, collapse = " + ")))
}

# Extract predictor names from a formula
predictors_from_formula <- function(frm) {
  attr(terms(frm), "term.labels")
}

# Metrics
# ==================== Evaluation ====================
safe_mape <- function(actual, predicted) {
  nz <- actual != 0
  if (!any(nz)) return(NA_real_)
  mean(abs((actual[nz] - predicted[nz]) / actual[nz])) * 100
}

evaluate_model <- function(actual, predicted) {
  n <- min(length(actual), length(predicted))
  a <- as.numeric(actual)[seq_len(n)]
  p <- as.numeric(predicted)[seq_len(n)]
  list(
    MAE  = mean(abs(a - p)),
    RMSE = sqrt(mean((a - p)^2)),
    MAPE = safe_mape(a, p),
    R2   = 1 - sum((a - p)^2) / sum((a - mean(a))^2)
  )
}

# NEW: evaluates each model against the correct "actual" (with optional overrides)
evaluate_models <- function(models,
                             default_actual,
                             time_index,
                             actual_overrides = list()) {
  library(data.table); library(ggplot2)
  
  # Metrics table
  evals <- rbindlist(lapply(names(models), function(m) {
    pred <- models[[m]]
    n <- length(pred)
    act_full <- if (!is.null(actual_overrides[[m]])) actual_overrides[[m]] else default_actual
    act_trim <- tail(act_full, n)
    as.data.table(evaluate_model(act_trim, pred))[, Model := m]
  }), fill = TRUE)
  setcolorder(evals, c("Model","MAE","RMSE","MAPE","R2"))
  print(evals)
  
  # Plot: align each series to its own "actual"
  plot_df <- data.table(Time = time_index)
  # Start with the default actual so we always have an "Actual" series
  plot_df[, Actual := default_actual]
  
  for (m in names(models)) {
    pred <- models[[m]]
    n <- length(pred)
    act_full <- if (!is.null(actual_overrides[[m]])) actual_overrides[[m]] else default_actual
    # right-align both to the test window
    plot_df[[paste0(m, "_pred")]] <- NA_real_
    plot_df[[paste0(m, "_act")]]  <- NA_real_
    plot_df[[paste0(m, "_pred")]][(nrow(plot_df)-n+1):nrow(plot_df)] <- pred
    plot_df[[paste0(m, "_act")]][(nrow(plot_df)-n+1):nrow(plot_df)]  <- tail(act_full, n)
  }
  
  # Long format just for predictions (overlay on a single axis)
  pred_cols <- grep("_pred$", names(plot_df), value = TRUE)
  long <- melt(plot_df[, c("Time", pred_cols), with = FALSE],
               id.vars = "Time", variable.name = "Series", value.name = "Prediction")
  long[, Model := sub("_pred$", "", Series)]
  
  ggplot() +
    geom_line(data = data.table(Time = time_index, Actual = default_actual),
              aes(Time, Actual), size = 0.7, color = "black") +
    geom_line(data = long, aes(Time, Prediction, color = Model), size = 0.7, alpha = 0.9) +
    labs(title = "Forecast vs Actual (aligned per model)", x = NULL, y = "kWh") +
    theme_minimal()
}

# ==================== Metric barplots ====================
# Single-metric horizontal barplot
plot_metric_bar <- function(evals, metric = c("MAE","RMSE","MAPE","R2")) {
  metric <- match.arg(metric)

  e <- as.data.table(evals)
  stopifnot(metric %in% names(e))
  
  # Sorting direction: lower is better for MAE/RMSE/MAPE; higher is better for R2
  if (metric == "R2") {
    e <- e[order(-get(metric), Model)]
  } else {
    e <- e[order(get(metric), Model)]
  }
  
  # Keep order in the plot
  e[, Model := factor(Model, levels = e$Model)]
  
  ggplot(e, aes(x = Model, y = .data[[metric]], fill = Model)) +
    geom_col(width = 0.7, alpha = 0.9) +
    coord_flip() +
    labs(title = paste("Model comparison -", metric),
         x = NULL, y = metric) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Convenience wrapper to draw all four metrics
plot_metrics_bars <- function(evals, which = c("MAE","RMSE","MAPE","R2")) {
  which <- intersect(which, names(evals))
  plots <- lapply(which, function(m) plot_metric_bar(evals, m))
  # Print them now (one below the other in typical RStudio plotting device)
  for (p in plots) print(p)
  invisible(plots)
}


# ==================== Plot only where predictions exist ====================
plot_aligned_forecasts <- function(models,
                                   default_actual,
                                   time_index,
                                   actual_overrides = list(),
                                   facet = TRUE) {
  library(data.table); library(ggplot2)
  
  # Build a long table containing ONLY the range each model predicts over
  segs <- rbindlist(lapply(names(models), function(m) {
    pred <- as.numeric(models[[m]])
    n    <- length(pred)
    
    # pick the right "actual" for this model (override if provided)
    act_full <- if (!is.null(actual_overrides[[m]])) actual_overrides[[m]] else default_actual
    act_seg  <- tail(act_full, n)
    time_seg <- tail(time_index, n)
    
    data.table(
      Model      = m,
      Time       = as.POSIXct(time_seg),
      Actual     = as.numeric(act_seg),
      Prediction = pred
    )
  }), fill = TRUE)
  
  # One combined plot (or facets per model)
  p <- ggplot(segs, aes(Time)) +
    geom_line(aes(y = Actual), size = 0.5, color = "black", alpha = 0.8) +
    geom_line(aes(y = Prediction, color = Model), size = 0.6, alpha = 0.9) +
    labs(title = "Forecast vs Actual (only where predictions exist)",
         x = NULL, y = "kWh") +
    theme_minimal()
  
  if (facet) {
    p <- p + facet_wrap(~ Model, ncol = 1, scales = "free_y") +
      theme(legend.position = "none")
  }
  print(p)
  invisible(p)
}

# ==================== Residual diagnostics (aligned per model) ====================
residual_diagnostics <- function(models,
                                 default_actual,
                                 time_index,
                                 actual_overrides = list()) {
  library(data.table); library(ggplot2)
  
  # Build residuals only over each model's prediction window
  resid_dt <- rbindlist(lapply(names(models), function(m) {
    pred <- as.numeric(models[[m]])
    n    <- length(pred)
    
    # choose correct actuals (override if provided)
    act_full <- if (!is.null(actual_overrides[[m]])) actual_overrides[[m]] else default_actual
    
    data.table(
      Model    = m,
      Time     = as.POSIXct(tail(time_index, n)),
      Actual   = as.numeric(tail(act_full, n)),
      Fitted   = pred
    )[, Residual := Actual - Fitted][]
  }), fill = TRUE, use.names = TRUE)
  
  # Summary stats per model
  resid_summary <- resid_dt[, .(
    Mean = mean(Residual, na.rm = TRUE),
    SD   = sd(Residual, na.rm = TRUE),
    Q05  = quantile(Residual, 0.05, na.rm = TRUE),
    Q50  = quantile(Residual, 0.50, na.rm = TRUE),
    Q95  = quantile(Residual, 0.95, na.rm = TRUE)
  ), by = Model]
  
  print(resid_summary)
  
  # Plot residuals over time, aligned
  p <- ggplot(resid_dt, aes(x = Time, y = Residual, color = Model)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(alpha = 0.9, linewidth = 0.5) +
    labs(title = "Residuals over time (aligned per model)",
         y = "Actual - Prediction", x = NULL) +
    theme_minimal()
  print(p)
  
  invisible(list(summary = resid_summary, data = resid_dt, plot = p))
}
