# --- Load libraries ---
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(tseries)
library(forecast)
library(viridis)
library(psych)

# --- Clean and prepare raw_data ---
raw_data <- raw_data[!is.na(raw_data$interval), ]  # Remove rows with missing timestamps
raw_data$interval <- as.POSIXct(raw_data$interval)
raw_data$date <- as.Date(raw_data$interval)

# Step 1: Convert to data.frame if needed
raw_df <- as.data.frame(raw_data)

# Step 2: Select only numeric columns
numeric_df <- raw_df[, sapply(raw_df, is.numeric)]

# Step 3: Generate summary statistics
summary_stats <- psych::describe(numeric_df)

# Step 4: Format and extract selected statistics
summary_stats_df <- summary_stats[, c("mean", "sd", "median", "min", "max", "skew", "kurtosis")]
summary_stats_df <- as.data.frame(summary_stats_df)
summary_stats_df <- tibble::rownames_to_column(summary_stats_df, var = "Variable")
summary_stats_df <- data.frame(summary_stats_df[1], round(summary_stats_df[-1], 2))

summary_stats_df <- round(summary_stats_df, 2)
# Step 5: View or export
print(summary_stats_df)

# --- Correlation Heatmap ---
selected_columns <- c(
  "total_consumption_kWh", "total_occupancy", "tempC", "humidity", "co2", 
  "sound", "lux", "T", "U", "RH", "M", "R", "S", "O", "Y"
)
cor_data <- raw_data[, ..selected_columns]
cor_matrix <- cor(cor_data, use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))
names(cor_df) <- c("Var1", "Var2", "Correlation")

ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis(option = "plasma", direction = -1, limits = c(-1, 1), name = "Correlation") +
  theme_bw(base_size = 13) +
  labs(title = "Correlation Heatmap: Sensor and Weather Variables", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Time Series Aggregation and Plots ---
# Daily
daily_data <- raw_data %>%
  group_by(date) %>%
  summarise(daily_kWh = sum(total_consumption_kWh, na.rm = TRUE))

ggplot(daily_data, aes(x = date, y = daily_kWh)) +
  geom_line(color = "#2c7bb6") +
  labs(title = "Daily Energy Consumption", x = "Date", y = "Energy (kWh)") +
  scale_x_date(date_labels = "%b %Y") +
  theme_light(base_size = 13)

# Weekly
weekly_data <- daily_data %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(weekly_kWh = sum(daily_kWh))

ggplot(weekly_data, aes(x = week, y = weekly_kWh)) +
  geom_line(color = "darkgreen") +
  labs(title = "Weekly Energy Consumption", x = "Week", y = "kWh") +
  theme_minimal(base_size = 12)

# Monthly
monthly_data <- daily_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(monthly_kWh = sum(daily_kWh))

ggplot(monthly_data, aes(x = month, y = monthly_kWh)) +
  geom_line(color = "darkorange") +
  labs(title = "Monthly Energy Consumption", x = "Month", y = "kWh") +
  theme_minimal(base_size = 12)

# --- Boxplots ---
# Monthly boxplot
raw_data$month <- factor(format(raw_data$interval, "%B"), levels = month.name)

ggplot(raw_data, aes(x = month, y = total_consumption_kWh)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Monthly Energy Consumption Distribution", x = "Month", y = "kWh") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Weekday boxplot
raw_data$weekday <- weekdays(raw_data$interval)
raw_data$weektype <- ifelse(weekdays(raw_data$interval) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

ggplot(raw_data, aes(x = weektype, y = total_consumption_kWh, fill = weektype)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Weekday" = "steelblue", "Weekend" = "tomato")) +
  labs(title = "Energy Consumption: Weekday vs Weekend", x = NULL, y = "kWh") +
  theme_minimal(base_size = 12)

# --- ACF/PACF & Stationarity Diagnostics ---
ts_daily <- ts(daily_data$daily_kWh, frequency = 7)

cat("=== Augmented Dickey-Fuller (ADF) Test ===\n")
print(adf.test(ts_daily, alternative = "stationary"))

par(mfrow = c(1, 2))
acf(ts_daily, lag.max = 30, main = "ACF - Daily Energy Consumption")
pacf(ts_daily, lag.max = 30, main = "PACF - Daily Energy Consumption")
par(mfrow = c(1, 1))
