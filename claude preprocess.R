# Energy Consumption Data Preprocessing Pipeline
# Capgemini Smart Office Building - Utrecht
# Author: [Your Name]
# Date: [Current Date]

# Load required libraries
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(VIM)  # for missing value visualization
library(forecast)  # for time series utilities
library(corrplot)  # for correlation analysis
library(zoo)  # for time series operations

# =====================================================
# STEP 1: DATA LOADING AND INITIAL INSPECTION
# =====================================================

# Assuming your data is already loaded as 'raw_data'
# If not, load it with: raw_data <- fread("your_data_file.csv")

cat("=== INITIAL DATA INSPECTION ===\n")
print(paste("Dataset dimensions:", nrow(raw_data), "x", ncol(raw_data)))
print("Structure of the dataset:")
str(raw_data)

# Check for completely missing rows
cat("\nRows with all NA values:", sum(apply(raw_data, 1, function(x) all(is.na(x)))), "\n")

# =====================================================
# STEP 2: HANDLE MISSING TIMESTAMPS
# =====================================================

cat("\n=== HANDLING MISSING TIMESTAMPS ===\n")

# Check for missing interval values
missing_intervals <- sum(is.na(raw_data$interval))
cat("Missing interval values:", missing_intervals, "\n")

# Remove rows with missing timestamps as they cannot be used for time series
if(missing_intervals > 0) {
  cat("Removing", missing_intervals, "rows with missing timestamps...\n")
  raw_data <- raw_data[!is.na(interval)]
}

# Convert interval to proper datetime
raw_data[, interval := as.POSIXct(interval, tz = "Europe/Amsterdam")]

# Check for duplicate timestamps
duplicates <- sum(duplicated(raw_data$interval))
if(duplicates > 0) {
  cat("Found", duplicates, "duplicate timestamps. Removing duplicates...\n")
  raw_data <- raw_data[!duplicated(interval)]
}

# Sort by timestamp
raw_data <- raw_data[order(interval)]

# =====================================================
# STEP 3: CREATE COMPLETE TIME SEQUENCE
# =====================================================

cat("\n=== CREATING COMPLETE TIME SEQUENCE ===\n")

# Determine the time range and frequency
start_time <- min(raw_data$interval, na.rm = TRUE)
end_time <- max(raw_data$interval, na.rm = TRUE)
cat("Data range:", as.character(start_time), "to", as.character(end_time), "\n")

# Create complete hourly sequence
complete_sequence <- seq(from = start_time, to = end_time, by = "hour")
cat("Expected number of hours:", length(complete_sequence), "\n")
cat("Actual number of records:", nrow(raw_data), "\n")

# Create complete time series framework
complete_data <- data.table(interval = complete_sequence)

# Merge with original data
processed_data <- merge(complete_data, raw_data, by = "interval", all.x = TRUE)

# =====================================================
# STEP 4: MISSING VALUE ANALYSIS AND TREATMENT
# =====================================================

cat("\n=== MISSING VALUE ANALYSIS ===\n")

# Calculate missing value percentages for each column
missing_summary <- processed_data[, lapply(.SD, function(x) sum(is.na(x))/length(x)*100)]
print("Missing value percentages by column:")
print(round(missing_summary, 2))

# Visualize missing patterns (optional - uncomment if needed)
# aggr(processed_data[, -"interval"], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE)

# Handle missing values based on variable type
cat("\nHandling missing values...\n")

# For energy consumption (target variable) - forward fill then backward fill
processed_data[, total_consumption_kWh := na.fill(total_consumption_kWh, c("extend", "extend"))]

# For indoor sensor data - linear interpolation within reasonable gaps
indoor_vars <- c("tempC", "humidity", "co2", "sound", "lux")
for(var in indoor_vars) {
  # Only interpolate gaps <= 6 hours to maintain data quality
  processed_data[, (var) := na.approx(get(var), maxgap = 6, na.rm = FALSE)]
  
  # For remaining NAs, use forward/backward fill
  processed_data[, (var) := na.fill(get(var), c("extend", "extend"))]
}

# For occupancy - assume 0 during missing periods (conservative approach)
processed_data[is.na(total_occupancy), total_occupancy := 0]

# For weather data - forward fill (weather changes gradually)
weather_vars <- c("temperature", "wind_speed", "sunshine_minutes", "global_radiation", 
                  "humidity_percent", "fog", "rain", "snow", "thunder", "ice")
for(var in weather_vars) {
  processed_data[, (var) := na.fill(get(var), c("extend", "extend"))]
}

# =====================================================
# STEP 5: OUTLIER DETECTION AND TREATMENT
# =====================================================

cat("\n=== OUTLIER DETECTION AND TREATMENT ===\n")

# Function to detect outliers using IQR method
detect_outliers <- function(x, k = 1.5) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - k * IQR
  upper <- Q3 + k * IQR
  return(x < lower | x > upper)
}
num_vars <- names(raw_data)[sapply(raw_data, is.numeric)]

# Check for outliers in key variables
key_vars <- c("total_consumption_kWh", "tempC", "humidity", "co2")
for(var in num_vars) {
  outliers <- detect_outliers(processed_data[[var]])
  cat("Outliers in", var, ":", sum(outliers, na.rm = TRUE), 
      "(", round(sum(outliers, na.rm = TRUE)/nrow(processed_data)*100, 2), "%)\n")
  
  # Cap extreme outliers (optional - adjust based on domain knowledge)
  if(var == "total_consumption_kWh") {
    # Cap consumption at 99th percentile (adjust as needed)
    cap_value <- quantile(processed_data[[var]], 0.99, na.rm = TRUE)
    processed_data[get(var) > cap_value, (var) := cap_value]
  }
}

library(data.table)

# Winsorization function that also returns count of capped values
winsorize_with_count <- function(x, probs = c(0.01, 0.99)) {
  if (!is.numeric(x)) return(list(x = x, n_capped = 0))
  
  bounds <- quantile(x, probs, na.rm = TRUE)
  lower <- bounds[1]
  upper <- bounds[2]
  
  # Count how many would be capped
  n_capped <- sum(x < lower | x > upper, na.rm = TRUE)
  
  # Apply winsorization
  x[x < lower] <- lower
  x[x > upper] <- upper
  
  return(list(x = x, n_capped = n_capped))
}


# --- Exclude binary/event variables ---
exclude_vars <- c("fog", "rain", "snow", "thunder", "ice")

num_vars <- setdiff(names(raw_data)[sapply(raw_data, is.numeric)], exclude_vars)

# --- Apply winsorization and collect results ---
winsor_summary <- data.table(variable = character(),
                             capped_obs = integer(),
                             capped_pct = numeric())

for (var in num_vars) {
  result <- winsorize_with_count(raw_data[[var]])
  raw_data[[var]] <- result$x
  
  winsor_summary <- rbind(winsor_summary, data.table(
    variable = var,
    capped_obs = result$n_capped,
    capped_pct = 100 * result$n_capped / nrow(raw_data)
  ))
}

# Print summary
print(winsor_summary)


# =====================================================
# STEP 6: FEATURE ENGINEERING
# =====================================================

cat("\n=== FEATURE ENGINEERING ===\n")

# Extract temporal features
processed_data[, `:=`(
  year = year(interval),
  month = month(interval),
  day = mday(interval),
  weekday = wday(interval, week_start = 1),  # Monday = 1
  hour = hour(interval),
  week_of_year = week(interval)
)]

# Create cyclical encoding for temporal features
processed_data[, `:=`(
  hour_sin = sin(2 * pi * hour / 24),
  hour_cos = cos(2 * pi * hour / 24),
  day_sin = sin(2 * pi * weekday / 7),
  day_cos = cos(2 * pi * weekday / 7),
  month_sin = sin(2 * pi * month / 12),
  month_cos = cos(2 * pi * month / 12)
)]

# Create lag features for energy consumption (important for forecasting)
processed_data[, `:=`(
  consumption_lag1 = shift(total_consumption_kWh, 1),
  consumption_lag24 = shift(total_consumption_kWh, 24),  # Same hour previous day
  consumption_lag168 = shift(total_consumption_kWh, 168) # Same hour previous week
)]

# Rolling averages
processed_data[, `:=`(
  consumption_ma24 = frollmean(total_consumption_kWh, 24, align = "right"),
  consumption_ma168 = frollmean(total_consumption_kWh, 168, align = "right"),
  temp_ma24 = frollmean(tempC, 24, align = "right")
)]

# Working hours indicator (assuming 6 AM to 11 PM based on your research)
processed_data[, working_hours := ifelse(hour >= 6 & hour <= 23, 1, 0)]

# Weekend indicator
processed_data[, weekend := ifelse(weekday %in% c(6, 7), 1, 0)]

# Create interaction terms
processed_data[, temp_occupancy := tempC * total_occupancy]
processed_data[, working_temp := working_hours * tempC]

# =====================================================
# STEP 7: DATA QUALITY CHECKS
# =====================================================

cat("\n=== FINAL DATA QUALITY CHECKS ===\n")

# Check for remaining missing values
final_missing <- processed_data[, lapply(.SD, function(x) sum(is.na(x)))]
vars_with_na <- names(final_missing)[final_missing > 0]
if(length(vars_with_na) > 0) {
  cat("Variables still with missing values:\n")
  print(final_missing[, vars_with_na, with = FALSE])
}

# Check for infinite or extremely large values
infinite_check <- processed_data[, lapply(.SD, function(x) sum(is.infinite(x)))]
if(sum(infinite_check) > 0) {
  cat("Infinite values found in:", names(infinite_check)[infinite_check > 0], "\n")
}

# Summary statistics
cat("\nSummary statistics for key variables:\n")
summary_vars <- c("total_consumption_kWh", "tempC", "humidity", "total_occupancy")
print(summary(processed_data[, summary_vars, with = FALSE]))

# =====================================================
# STEP 8: SAVE PROCESSED DATA
# =====================================================

cat("\n=== SAVING PROCESSED DATA ===\n")

# Create different datasets for different forecasting horizons
# Full dataset
fwrite(processed_data, "processed_energy_data_full.csv")

# Remove initial rows with NAs from lag features for modeling
model_ready_data <- processed_data[complete.cases(processed_data[, c("total_consumption_kWh", 
                                                                     "consumption_lag1", 
                                                                     "consumption_lag24")])]

fwrite(model_ready_data, "processed_energy_data_model_ready.csv")

cat("Data preprocessing completed successfully!\n")
cat("Final dataset dimensions:", nrow(model_ready_data), "x", ncol(model_ready_data), "\n")
cat("Files saved: processed_energy_data_full.csv and processed_energy_data_model_ready.csv\n")

# =====================================================
# STEP 9: EXPLORATORY DATA ANALYSIS
# =====================================================

cat("\n=== CREATING VISUALIZATION PLOTS ===\n")

# Time series plot of energy consumption
p1 <- ggplot(processed_data, aes(x = interval, y = total_consumption_kWh)) +
  geom_line(color = "blue", alpha = 0.7) +
  labs(title = "Energy Consumption Over Time", 
       x = "Time", y = "Consumption (kWh)") +
  theme_minimal()

# Daily pattern
daily_pattern <- processed_data[, .(avg_consumption = mean(total_consumption_kWh, na.rm = TRUE)), 
                                by = hour]
p2 <- ggplot(daily_pattern, aes(x = hour, y = avg_consumption)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red") +
  labs(title = "Average Energy Consumption by Hour", 
       x = "Hour of Day", y = "Average Consumption (kWh)") +
  theme_minimal()

# Weekly pattern
weekly_pattern <- processed_data[, .(avg_consumption = mean(total_consumption_kWh, na.rm = TRUE)), 
                                 by = weekday]
p3 <- ggplot(weekly_pattern, aes(x = factor(weekday, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                                 y = avg_consumption)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Average Energy Consumption by Day of Week", 
       x = "Day of Week", y = "Average Consumption (kWh)") +
  theme_minimal()

# Print the plots
print(p1)
print(p2)
print(p3)

# Correlation matrix for key variables
numeric_vars <- c("total_consumption_kWh", "tempC", "humidity", "co2", "total_occupancy", 
                  "temperature", "wind_speed", "global_radiation")
cor_matrix <- cor(processed_data[, numeric_vars, with = FALSE], use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         title = "Correlation Matrix - Key Variables", mar = c(0,0,1,0))

cat("\nPreprocessing pipeline completed! You can now proceed with model development.\n")

