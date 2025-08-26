# ================================================================
# Energy + Explanatory Variables Full Analysis Pipeline
# ================================================================

# ---------------------- Libraries -------------------------------
library(dplyr)
library(data.table)
library(psych)
library(ggplot2)
library(zoo)

# ------------------------------------------------
# 1. Merge Energy + Raw Data
# ------------------------------------------------
energy_dt <- as.data.table(energy_hourly) %>%
  rename(interval = hour)

raw_dt <- as.data.table(raw_data)[!is.na(interval)]

processed_data <- merge(
  energy_dt, raw_dt,
  by = "interval",
  all.x = TRUE
)

# ------------------------------------------------
# 2. Descriptive Statistics (Before Preprocessing)
# ------------------------------------------------
numeric_cols <- names(processed_data)[sapply(processed_data, is.numeric)]

summary_stats_before <- psych::describe(as.data.frame(processed_data[, ..numeric_cols]))
summary_stats_before_df <- data.frame(
  Variable = rownames(summary_stats_before),
  round(summary_stats_before[, c("mean","sd","median","min","max","skew","kurtosis")], 2),
  row.names = NULL,
  check.names = FALSE
)

cat("\n=== DESCRIPTIVE STATISTICS (BEFORE PREPROCESSING) ===\n")
print(summary_stats_before_df)

# ------------------------------------------------
# 3. Outlier Detection Plots (Before Capping)
# ------------------------------------------------
detect_outliers <- function(x, k = 1.5) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - k*IQR
  upper <- Q3 + k*IQR
  return(x < lower | x > upper)
}

# Non-binary numeric variables
non_binary_vars <- numeric_cols[
  sapply(processed_data[, ..numeric_cols], function(x) length(unique(na.omit(x))) > 2)
]

outlier_plot_dt <- rbindlist(lapply(non_binary_vars, function(var) {
  flags <- detect_outliers(processed_data[[var]])
  data.table(
    interval = processed_data$interval,
    Variable = var,
    Value = processed_data[[var]],
    Outlier = flags
  )
}), fill = TRUE)

p_outlier_all <- ggplot(outlier_plot_dt, aes(x = interval, y = Value)) +
  geom_line(color = "skyblue3", linewidth = 0.3, na.rm = TRUE) +
  geom_point(data = outlier_plot_dt[Outlier == TRUE],
             aes(x = interval, y = Value),
             color = "red", size = 0.8, na.rm = TRUE) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  labs(title = "Time Series with Outliers Highlighted (Before Capping)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11)
print(p_outlier_all)

# ------------------------------------------------
# 4. Missing Value Analysis & Treatment
# ------------------------------------------------

# ------------------------------------------------
# Missing Value Analysis
# ------------------------------------------------
cat("\n=== MISSING VALUE ANALYSIS ===\n")

# Count and percentage of missing values
missing_counts <- processed_data[, lapply(.SD, function(x) sum(is.na(x)))]
missing_perc   <- processed_data[, lapply(.SD, function(x) mean(is.na(x)) * 100)]

# Combine into a single data.table
missing_summary <- data.table(
  Variable        = names(missing_counts),
  Missing_Count   = as.numeric(missing_counts[1, ]),
  Missing_Percent = as.numeric(missing_perc[1, ])
)

# Show only variables with missing values
missing_summary <- missing_summary[Missing_Count > 0][order(-Missing_Percent)]

cat("Missing values by variable (counts & %):\n")
print(missing_summary)

# ------------------------------------------------
# Inspect observations with missing values
# ------------------------------------------------
rows_with_na <- processed_data[!complete.cases(processed_data)]

cat("\n=== ROWS WITH MISSING VALUES ===\n")
# Add block grouping based on gaps > 1 hour in full timeline
rows_with_na[, block := cumsum(
  c(1, as.numeric(diff(interval), units = "hours") > 1)
)]

na_blocks <- rows_with_na[, .(
  start = min(interval),
  end   = max(interval),
  n_hours = .N
), by = block]

print(na_blocks)


cat("\n=== MISSING VALUE ANALYSIS ===\n")
missing_summary <- processed_data[, lapply(.SD, function(x) sum(is.na(x))/length(x)*100)]
print("Missing value percentages by column:")
print(round(missing_summary, 2))

cat("\nHandling missing values...\n")

# Energy: forward/back fill
processed_data[, total_consumption_kWh := na.fill(total_consumption_kWh, c("extend","extend"))]

# Indoor sensor vars
indoor_vars <- c("tempC","humidity","co2","sound","lux")
for (var in intersect(indoor_vars, names(processed_data))) {
  processed_data[, (var) := na.approx(get(var), maxgap = 6, na.rm = FALSE)]
  processed_data[, (var) := na.fill(get(var), c("extend","extend"))]
}

# Occupancy: assume 0 when missing
if ("total_occupancy" %in% names(processed_data)) {
  processed_data[is.na(total_occupancy), total_occupancy := 0]
}

# Weather: forward fill
weather_vars <- c("temperature","wind_speed","sunshine_minutes","global_radiation",
                  "humidity_percent","fog","rain","snow","thunder","ice")
for (var in intersect(weather_vars, names(processed_data))) {
  processed_data[, (var) := na.fill(get(var), c("extend","extend"))]
}

# ------------------------------------------------
# 5. Outlier Winsorization (CO2 + Sound)
# ------------------------------------------------
co2_upper   <- quantile(processed_data$co2,   0.999, na.rm = TRUE)
sound_lower <- quantile(processed_data$sound, 0.001, na.rm = TRUE)
sound_upper <- quantile(processed_data$sound, 0.999, na.rm = TRUE)

cat("\nCO2 upper cutoff (99.9%):", co2_upper, "\n")
cat("Sound lower cutoff (0.1%):", sound_lower, "\n")
cat("Sound upper cutoff (99.9%):", sound_upper, "\n")

# Apply capping
processed_data[co2 > co2_upper, co2 := co2_upper]
processed_data[sound > sound_upper, sound := sound_upper]
processed_data[sound < sound_lower, sound := sound_lower]

# ------------------------------------------------
# 6. Descriptive Statistics (After Preprocessing)
# ------------------------------------------------
summary_stats_after <- psych::describe(as.data.frame(processed_data[, ..numeric_cols]))
summary_stats_after_df <- data.frame(
  Variable = rownames(summary_stats_after),
  round(summary_stats_after[, c("mean","sd","median","min","max","skew","kurtosis")], 2),
  row.names = NULL,
  check.names = FALSE
)

cat("\n=== DESCRIPTIVE STATISTICS (AFTER PREPROCESSING) ===\n")
print(summary_stats_after_df)

# ------------------------------------------------
# 7. Final Data Quality Checks
# ------------------------------------------------
cat("\n=== FINAL DATA QUALITY CHECKS ===\n")

final_missing <- processed_data[, lapply(.SD, function(x) sum(is.na(x)))]
vars_with_na <- names(final_missing)[final_missing > 0]
if (length(vars_with_na) > 0) {
  cat("Variables still with missing values:\n")
  print(final_missing[, vars_with_na, with=FALSE])
} else {
  cat("No missing values remaining.\n")
}

infinite_check <- processed_data[, lapply(.SD, function(x) sum(is.infinite(x)))]
if (sum(infinite_check) > 0) {
  cat("Infinite values found in:", names(infinite_check)[infinite_check > 0], "\n")
} else {
  cat("No infinite values found.\n")
}

# ------------------------------------------------
# 8. Correlation Analysis
# ------------------------------------------------
# Heatmap
cor_matrix <- cor(processed_data[, ..numeric_cols], use = "pairwise.complete.obs")
cor_dt <- as.data.table(as.table(cor_matrix))
setnames(cor_dt, old = names(cor_dt), new = c("Var1", "Var2", "Correlation"))

p_corr <- ggplot(cor_dt, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(p_corr)

# Correlation with energy consumption
non_binary_vars <- setdiff(non_binary_vars, "total_consumption_kWh")

energy_corr <- sapply(processed_data[, ..non_binary_vars], function(x) {
  cor(processed_data$total_consumption_kWh, x, use = "pairwise.complete.obs")
})

energy_corr_dt <- data.table(
  Variable = names(energy_corr),
  Correlation = as.numeric(energy_corr)
)

p_energy_corr <- ggplot(energy_corr_dt, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Correlation with Energy Consumption",
       x = NULL, y = "Correlation") +
  theme_minimal(base_size = 12)
print(p_energy_corr)

# ------------------------------------------------
# 9. Visualization
# ------------------------------------------------
# Time Series Plots (clean data)
plot_dt <- melt(
  processed_data,
  id.vars = "interval",
  measure.vars = setdiff(names(processed_data), c("interval", "total_consumption_kWh")),
  variable.name = "Variable",
  value.name = "Value"
)

p_all_ts <- ggplot(plot_dt, aes(interval, Value)) +
  geom_line(linewidth = 0.25, color = "skyblue3", na.rm = TRUE) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  labs(title = "Time Series of Explanatory Variables", x = NULL, y = NULL) +
  theme_minimal(base_size = 11)
print(p_all_ts)

# Scatter plots: Energy vs explanatory variables
scatter_dt <- melt(
  processed_data,
  id.vars = "total_consumption_kWh",
  measure.vars = non_binary_vars,
  variable.name = "Variable",
  value.name = "Value"
)

p_scatter <- ggplot(scatter_dt, aes(x = Value, y = total_consumption_kWh)) +
  geom_point(alpha = 0.3, size = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 3) +
  labs(title = "Scatter Plots: Energy vs Explanatory Variables",
       x = NULL, y = "Energy Consumption (kWh)") +
  theme_minimal(base_size = 11)
print(p_scatter)

