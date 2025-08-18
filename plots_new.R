# ────────────────────────────────────────────────────────────────────────────────
# Load Required Libraries
# ────────────────────────────────────────────────────────────────────────────────
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(forecast)
library(psych)
library(viridis)
library(tibble)
library(zoo)
library(lubridate)
library(patchwork)
library(Amelia)
library(VIM)

# ────────────────────────────────────────────────────────────────────────────────
# Data Cleaning & Preparation
# ────────────────────────────────────────────────────────────────────────────────
raw_data <- raw_data[!is.na(raw_data$interval), ]
raw_data$interval <- as.POSIXct(raw_data$interval)
raw_data$date <- as.Date(raw_data$interval)

# ────────────────────────────────────────────────────────────────────────────────
# Descriptive Statistics
# ────────────────────────────────────────────────────────────────────────────────
numeric_cols <- names(raw_data)[sapply(raw_data, is.numeric)]
temporal_cols <- c("^lag_", "^rollmean_", "^is_", "^hour_", "^office_", "holiday", "hour")
raw_numeric_cols <- numeric_cols[!grepl(paste(temporal_cols, collapse = "|"), numeric_cols)]
raw_data_subset <- raw_data[, raw_numeric_cols, with = FALSE]

summary_stats <- psych::describe(raw_data_subset)
summary_stats_df <- summary_stats[, c("mean", "sd", "median", "min", "max", "skew", "kurtosis")]
summary_stats_df <- as.data.frame(summary_stats_df)
summary_stats_df <- rownames_to_column(summary_stats_df, var = "Variable")
summary_stats_df[,-1] <- round(summary_stats_df[,-1], 2)
print(summary_stats_df)

# ────────────────────────────────────────────────────────────────────────────────
# Time Series Plots
# ────────────────────────────────────────────────────────────────────────────────


# Ensure POSIXct datetime column is available
raw_data$datetime <- as.POSIXct(raw_data$interval)

# Plot of hourly energy consumption
p_hourly_ts <- ggplot(raw_data, aes(x = datetime, y = total_consumption_kWh)) +
  geom_line(color = "#0072B2", alpha = 0.8) +
  labs(
    title = "Time Series of Hourly Energy Consumption",
    x = "Datetime", y = "Energy Consumption (kWh)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Daily aggregated data
daily_kWh <- raw_data %>%
  mutate(date = as.Date(interval)) %>%
  group_by(date) %>%
  summarise(daily_kWh = sum(total_consumption_kWh, na.rm = TRUE)) %>%
  ungroup()

# 1. Daily time series
p_time_series <- ggplot(daily_kWh, aes(x = date, y = daily_kWh)) +
  geom_line(color = "#E69F00") +
  labs(title = "Time Series of Daily Energy Consumption", x = "Date", y = "Energy Consumption (kWh)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

# 2. Hourly energy consumption for January and July
p_jan <- raw_data %>%
  filter(interval >= as.POSIXct("2024-01-01") & interval <= as.POSIXct("2024-02-01")) %>%
  ggplot(aes(x = interval, y = total_consumption_kWh)) +
  geom_line(color = "#0072B2") +
  labs(title = "Hourly Energy Consumption (Jan 2024)", x = "Time", y = "Consumption (kWh)") +
  theme_minimal()

p_jul <- raw_data %>%
  filter(interval >= as.POSIXct("2024-07-01") & interval <= as.POSIXct("2024-08-01")) %>%
  ggplot(aes(x = interval, y = total_consumption_kWh)) +
  geom_line(color = "#0072B2") +
  labs(title = "Hourly Energy Consumption (July 2024)", x = "Time", y = "Consumption (kWh)") +
  theme_minimal()

# 3. Hourly consumption by month
raw_data$month <- format(raw_data$interval, "%Y-%m")
p_monthly <- ggplot(raw_data, aes(x = interval, y = total_consumption_kWh)) +
  geom_line(color = "#0072B2", alpha = 0.5) +
  facet_wrap(~ month, scales = "free_x", ncol = 3) +
  labs(title = "Hourly Energy Use by Month", x = "Time", y = "kWh") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ────────────────────────────────────────────────────────────────────────────────
# Boxplots by Weekday & Month
# ────────────────────────────────────────────────────────────────────────────────
Sys.setlocale("LC_TIME", "C")

daily_kWh <- daily_kWh %>%
  mutate(
    month = format(date, "%Y-%m"),
    weekday = wday(date, label = TRUE, abbr = FALSE, week_start = 1),
    weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  )

# 1. Boxplot by weekday
p_box_weekday <- ggplot(daily_kWh, aes(x = weekday, y = daily_kWh)) +
  geom_boxplot(fill = "#E69F00", color = "grey30", outlier.colour = "firebrick") +
  labs(title = "Boxplot of Daily Energy by Weekday", x = "Weekday", y = "Daily Energy (kWh)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 2. Boxplot by month
p_box_month <- ggplot(daily_kWh, aes(x = month, y = daily_kWh)) +
  geom_boxplot(fill = "#56B4E9", color = "grey30", outlier.colour = "firebrick") +
  labs(title = "Boxplot of Daily Energy by Month", x = "Month", y = "Daily Energy (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

# ────────────────────────────────────────────────────────────────────────────────
# Correlation Heatmap (non-engineered variables)
# ────────────────────────────────────────────────────────────────────────────────
cor_matrix <- cor(raw_data[, ..raw_numeric_cols], use = "complete.obs")
cor_df <- melt(cor_matrix)
colnames(cor_df) <- c("Var1", "Var2", "Correlation")

p_corr <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

# ────────────────────────────────────────────────────────────────────────────────
# ACF and PACF Plots
# ────────────────────────────────────────────────────────────────────────────────
energy_ts <- ts(raw_data$total_consumption_kWh, frequency = 24)

par(mfrow = c(1, 2))
acf(energy_ts, main = "ACF: Hourly Energy", lag.max = 168)
pacf(energy_ts, main = "PACF: Hourly Energy", lag.max = 168)
par(mfrow = c(1, 1))

# ────────────────────────────────────────────────────────────────────────────────
# Display All Plots
# ────────────────────────────────────────────────────────────────────────────────
print(p_hourly_ts)
print(p_time_series)
print(p_jan)
print(p_jul)
print(p_monthly)
print(p_daily_avg)
print(p_box_month)
print(p_box_weekday)
print(p_corr)

