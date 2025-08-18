# q.data_exploration_plots.R
# -----------------------------------------------------------------------------
# Script to plot data for initial exploration
# -----------------------------------------------------------------------------

# --- Data Cleaning & Preparation --- 

hourly_data <- as.data.table(raw_data)
hourly_data <- hourly_data[!is.na(interval)]
hourly_data[, interval := as.POSIXct(interval,
                                tz = "UTC",
                                format = "%Y-%m-%d %H:%M:%S")]
# Keep only 2023–2024
hourly_data <- hourly_data[
  interval >= as.POSIXct("2023-01-01 00:00:00", tz = "UTC") &
    interval <  as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
]
hourly_data[, date       := as.Date(interval)]
hourly_data[, hour       := hour(interval)]
hourly_data[, month_str  := format(interval, "%Y-%m")]
hourly_data[, weekday_en := wday(date, label = TRUE, abbr = FALSE, week_start = 1, locale = "C")]
hourly_data[, weekday_en := factor(
  weekday_en,
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)]

# --- Descriptive Statistics --- 

numeric_cols <- names(hourly_data)[vapply(hourly_data, is.numeric, logical(1L))]
temporal_cols <- c("^lag_", "^rollmean_", "^is_", "^hour_", "^office_", "holiday", "hour")
raw_numeric_cols <- numeric_cols[!grepl(paste(temporal_cols, collapse = "|"), numeric_cols)]

summary_stats <- psych::describe(as.data.frame(hourly_data[, ..raw_numeric_cols]))
summary_stats_df <- data.frame(
  Variable = rownames(summary_stats),
  round(summary_stats[, c("mean","sd","median","min","max","skew","kurtosis")], 2),
  row.names = NULL,
  check.names = FALSE
)
print(summary_stats_df)

# --- Daily aggregation --- 
daily_kWh <- hourly_data[, .(daily_kWh = sum(total_consumption_kWh, na.rm = TRUE)), by = date]
daily_kWh[, month_str  := format(date, "%Y-%m")]
daily_kWh[, weekday_en := wday(date, label = TRUE, abbr = FALSE, week_start = 1, locale = "C")]
daily_kWh[, weekday_en := factor(
  weekday_en,
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)]

# --- Time Series Plots ---

# Hourly time series
p_hourly_ts <- ggplot(hourly_data, aes(x = interval, y = total_consumption_kWh)) +
  geom_line(color = "#0072B2", alpha = 0.8) +
  labs(title = "Time Series of Hourly Energy Consumption",
       x = "Datetime", y = "Energy Consumption (kWh)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Daily time series
p_time_series <- ggplot(daily_kWh, aes(x = date, y = daily_kWh)) +
  geom_line(color = "#E69F00") +
  labs(title = "Time Series of Daily Energy Consumption",
       x = "Date", y = "Energy Consumption (kWh)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Hourly energy consumption for January and July 2024
p_jan <- hourly_data[month_str == "2024-01"] |>
  ggplot(aes(x = interval, y = total_consumption_kWh)) +
  geom_line(color = "#0072B2") +
  labs(title = "Hourly Energy Consumption (Jan 2024)",
       x = "Time", y = "Consumption (kWh)") +
  theme_minimal()

p_jul <- hourly_data[month_str == "2024-07"] |>
  ggplot(aes(x = interval, y = total_consumption_kWh)) +
  geom_line(color = "#0072B2") +
  labs(title = "Hourly Energy Consumption (July 2024)",
       x = "Time", y = "Consumption (kWh)") +
  theme_minimal()

# Hourly by month 
p_monthly <- ggplot(hourly_data, aes(x = interval, y = total_consumption_kWh)) +
  geom_line(color = "#0072B2", alpha = 0.5) +
  facet_wrap(~ month_str, scales = "free_x", ncol = 3) +
  labs(title = "Hourly Energy Use by Month", x = "Time", y = "kWh") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Boxplots ---

p_box_weekday <- ggplot(daily_kWh, aes(x = weekday_en, y = daily_kWh)) +
  geom_boxplot(fill = "#E69F00", color = "grey30", outlier.colour = "firebrick") +
  labs(title = "Boxplot of Daily Energy by Weekday", x = "Weekday", y = "Daily Energy (kWh)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p_box_month <- ggplot(daily_kWh, aes(x = month_str, y = daily_kWh)) +
  geom_boxplot(fill = "#56B4E9", color = "grey30", outlier.colour = "firebrick") +
  labs(title = "Boxplot of Daily Energy by Month", x = "Month", y = "Daily Energy (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

p_box_hour <- ggplot(hourly_data, aes(x = factor(hour), y = total_consumption_kWh)) +
  geom_boxplot(fill = "#009E73", color = "grey30", outlier.colour = "firebrick") +
  labs(title = "Boxplot of Hourly Energy by Hour of Day",
       x = "Hour of Day", y = "Hourly Energy (kWh)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# --- Correlation Heatmap ---

cor_dt <- as.data.table(as.table(cor_matrix))
setnames(cor_dt, old = names(cor_dt), new = c("Var1", "Var2", "Correlation"))

p_corr <- ggplot(cor_dt, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# --- ACF and PACF Plots ---

energy_ts <- ts(hourly_data$total_consumption_kWh, frequency = 24)
par(mfrow = c(1, 2))
acf(energy_ts, main = "ACF: Hourly Energy", lag.max = 24 * 14) # max lag = two weeks
pacf(energy_ts, main = "PACF: Hourly Energy", lag.max = 24 * 14)
par(mfrow = c(1, 1))

# --- Display All Plots ---

print(p_hourly_ts)
print(p_time_series)
print(p_jan)
print(p_jul)
print(p_monthly)
print(p_box_month)
print(p_box_weekday)
print(p_box_hour)
print(p_corr)
