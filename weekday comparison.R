# ==== Weekday error analysis ====
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(lubridate)
})

# --- Define model colors ---
model_cols <- c(
  "HYBRID_SARIMAX+LSTM" = "coral",
  "LSTM"                = "seagreen",
  "SARIMAX"             = "mediumpurple1",
  "SARIMA"              = "royalblue",
  "XGBOOST"           = "goldenrod1" , # excluded
  "HYBRID_SARIMAX+XGB" = "darkred"
)

# --- 0) Combine folds if not already combined ---
if (!exists("all_results")) {
  stopifnot(exists("all_res"))
  all_results <- rbindlist(all_res, use.names = TRUE)
}

# --- 1) Local timezone for weekday definition ---
LOCAL_TZ <- "Europe/Amsterdam"
all_results[, interval_local := with_tz(interval, LOCAL_TZ)]

# --- 2) Add weekday (English) & residuals ---
all_results[, weekday := wday(interval_local,
                              label = TRUE,
                              abbr  = TRUE,             # TRUE = "Mon","Tue"...; FALSE = "Monday","Tuesday"...
                              week_start = 1,
                              locale = "en_US.UTF-8")]  # force English
all_results[, residual := actual - forecast]

# --- 3) Metrics by weekday x model x fold ---
eps <- 1e-6
wd_metrics_fold <- all_results[, .(
  RMSE  = sqrt(mean(residual^2, na.rm = TRUE)),
  MAE   = mean(abs(residual), na.rm = TRUE),
  MAPE  = mean(abs(residual/actual)[abs(actual) > eps], na.rm = TRUE) * 100,
  sMAPE = mean(200 * abs(residual) / (abs(actual) + abs(forecast) + eps), na.rm = TRUE)
), by = .(fold, model, weekday)][order(fold, model, weekday)]

print(wd_metrics_fold)

# --- 4) Metrics aggregated over folds ---
wd_metrics_all <- wd_metrics_fold[, .(
  RMSE  = mean(RMSE),
  MAE   = mean(MAE),
  MAPE  = mean(MAPE),
  sMAPE = mean(sMAPE)
), by = .(model, weekday)][order(model, weekday)]

# drop XGBOOST if present
wd_metrics_all <- wd_metrics_all[model %in% names(model_cols)]

print(wd_metrics_all)

# --- 5) Plots ---
# MAE
p_mae <- ggplot(wd_metrics_all, aes(x = weekday, y = MAE, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = model_cols) +
  labs(title = "MAE by Weekday (averaged over folds)",
       x = "Weekday", y = "MAE (kWh)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
print(p_mae)

# MAPE
p_mape <- ggplot(wd_metrics_all, aes(x = weekday, y = MAPE, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = model_cols) +
  labs(title = "MAPE by Weekday (averaged over folds)",
       x = "Weekday", y = "MAPE (%)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
print(p_mape)

# RMSE
p_rmse <- ggplot(wd_metrics_all, aes(x = weekday, y = RMSE, fill = model)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = model_cols) +
  labs(title = "RMSE by Weekday (averaged over folds)",
       x = "Weekday", y = "RMSE (kWh)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
print(p_rmse)

# --- 6) Table of best performers ---
best_by_weekday <- wd_metrics_all[, .SD[which.min(MAE)], by = weekday]
print(best_by_weekday)
