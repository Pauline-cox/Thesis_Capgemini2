# ================================================================
# 1. Identify and Plot Issues - Full Period (2023-01 to 2024-12)
# ================================================================
# ------------------------------------------------
# 1. Load raw Eneco data
# ------------------------------------------------
energy_2023 <- read_excel("Data/Eneco/Kantoorgebouw K20016264_2023_871687460008204791_E (1).xlsx")
energy_2024 <- read_excel("Data/Eneco/Kantoorgebouw - K20016264_2024_871687460008204791_E (1).xlsx")

energy_raw <- bind_rows(energy_2023, energy_2024) %>%
  mutate(total_consumption = `Levering Dal` + `Levering Piek`)

cat("Loaded raw data: ", nrow(energy_raw), " rows\n")


cat("\n=== Missing / Anomalous Blocks - Full Period (2023–2024) ===\n")

raw_seq_full <- tibble(`Start Datum` = seq(
  min(energy_raw$`Start Datum`),
  max(energy_raw$`Start Datum`),
  by = "15 min"
))

# ================================================================
# 1. Identify Missing vs. Low Obs - Full Period (2023–2024)
# ================================================================

cat("\n=== Missing / Low Blocks - Full Period (2023–2024) ===\n")

check_dt_full <- raw_seq_full %>%
  left_join(
    dplyr::select(energy_raw, `Start Datum`, total_consumption),
    by = "Start Datum"
  ) %>%
  arrange(`Start Datum`) %>%
  mutate(
    issue_type = case_when(
      is.na(total_consumption)            ~ "missing",
      total_consumption == 0             ~ "zero",
      TRUE                                ~ "ok"
    ),
    flag_issue = ifelse(issue_type == "ok", 0, 1)
  )

# Collapse consecutive problem points into blocks
issue_blocks_full <- check_dt_full %>%
  mutate(block = cumsum(c(0, diff(flag_issue)) != 0)) %>%
  group_by(block) %>%
  summarise(
    start = min(`Start Datum`),
    end   = max(`Start Datum`),
    n     = sum(flag_issue),
    issue_types = paste(unique(issue_type[issue_type != "ok"]), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n > 0)

print(issue_blocks_full, n = Inf)


# Plot full period with shaded anomalies
p_highlighted_full <- ggplot(check_dt_full, aes(x = `Start Datum`, y = total_consumption)) +
  geom_rect(data = issue_blocks_full,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "black", alpha = 0.4) +
  geom_line(color = "skyblue3", alpha = 0.6) +
  labs(
    title = "Full Series with Missing/Zero Periods Highlighted",
    x = "Datetime", y = "Consumption (kWh)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

print(p_highlighted_full)


# ================================================================
# 2. Restrict to Sept 2023 onward and repeat
# ================================================================

cat("\n=== Restricting to Sept 2023 onward ===\n")

energy_all <- energy_raw %>%
  filter(`Start Datum` >= as.POSIXct("2023-09-01 00:00:00", tz = "UTC"))

raw_seq <- tibble(`Start Datum` = seq(
  min(energy_all$`Start Datum`),
  max(energy_all$`Start Datum`),
  by = "15 min"
))

check_dt <- raw_seq %>%
  left_join(
    dplyr::select(energy_all, `Start Datum`, total_consumption),
    by = "Start Datum"
  ) %>%
  arrange(`Start Datum`) %>%
  mutate(
    flag_issue = ifelse(is.na(total_consumption) | total_consumption == 0, 1, 0)
  )

issue_blocks <- check_dt %>%
  mutate(block = cumsum(c(0, diff(flag_issue)) != 0)) %>%
  group_by(block) %>%
  summarise(
    start = min(`Start Datum`),
    end   = max(`Start Datum`),
    issue = first(flag_issue),
    n     = sum(flag_issue),
    .groups = "drop"
  ) %>%
  filter(issue == 1)

print(issue_blocks, n = Inf)

# ------------------------------------------------
# 4. Fill anomalies (≤10 kWh) with 2-week profile
# ------------------------------------------------
fill_anomalies_with_profile <- function(df, min_valid = 0) {
  df <- df %>%
    arrange(`Start Datum`) %>%
    mutate(date = as.Date(`Start Datum`),
           weekday = wday(date, week_start = 1),
           hour = hour(`Start Datum`),
           minute = minute(`Start Datum`))
  
  anomaly_idx <- which(df$total_consumption <= min_valid)
  cat("Filling", length(anomaly_idx), "anomalous obs (≤", min_valid, "kWh)\n")
  
  for (i in anomaly_idx) {
    ts <- df$`Start Datum`[i]
    wday_i <- df$weekday[i]; h_i <- df$hour[i]; m_i <- df$minute[i]
    window_start <- ts - weeks(2); window_end <- ts + weeks(2)
    
    ref_vals <- df %>%
      filter(`Start Datum` >= window_start,
             `Start Datum` <= window_end,
             weekday == wday_i, hour == h_i, minute == m_i,
             total_consumption > min_valid) %>%
      pull(total_consumption)
    
    if (length(ref_vals) > 0) {
      new_val <- mean(ref_vals, na.rm = TRUE)
      cat(" Replacing", ts, "with", round(new_val, 2), "kWh (profile avg)\n")
      df$total_consumption[i] <- new_val
    } else {
      cat(" No reference values for", ts, "-> set NA\n")
      df$total_consumption[i] <- NA
    }
  }
  df
}

energy_clean <- fill_anomalies_with_profile(energy_all, min_valid = 0)

# ------------------------------------------------
# 5. Fill short gaps with linear interpolation
# ------------------------------------------------
full_seq <- tibble(`Start Datum` = seq(min(energy_clean$`Start Datum`),
                                       max(energy_clean$`Start Datum`),
                                       by = "15 min"))

energy_full <- full_seq %>%
  left_join(energy_clean %>% dplyr::select(`Start Datum`, total_consumption),
            by = "Start Datum") %>%
  arrange(`Start Datum`)

n_before <- sum(is.na(energy_full$total_consumption))
energy_full <- energy_full %>%
  mutate(total_consumption = na.approx(total_consumption, na.rm = FALSE))
n_after <- sum(is.na(energy_full$total_consumption))

cat("Linear interpolation filled", n_before - n_after, "missing values\n")

# print which timestamps were interpolated
interp_rows <- energy_full %>%
  filter(is.na(total_consumption)) %>% nrow()
if (interp_rows > 0) {
  cat(" Remaining NA rows after interpolation:", interp_rows, "\n")
}

# ------------------------------------------------
# 6. Aggregate to hourly
# ------------------------------------------------
energy_hourly <- energy_full %>%
  mutate(hour = floor_date(`Start Datum`, "hour")) %>%
  group_by(hour) %>%
  summarise(total_consumption_kWh = sum(total_consumption, na.rm = TRUE),
            .groups = "drop")

hourly_data <- as.data.table(energy_hourly) %>%
  rename(interval = hour)

# ------------------------------------------------
# 7. Feature engineering for plots
# ------------------------------------------------
hourly_data[, date := as.Date(interval)]
hourly_data[, hour := hour(interval)]
hourly_data[, month_str := format(interval, "%Y-%m")]
hourly_data[, weekday_en := wday(date, label = TRUE, abbr = FALSE,
                                 week_start = 1, locale = "C")]
hourly_data[, weekday_en := factor(weekday_en,
                                   levels = c("Monday","Tuesday","Wednesday","Thursday",
                                              "Friday","Saturday","Sunday"))]

# ------------------------------------------------
# 8. Descriptive Statistics
# ------------------------------------------------
summary_stats <- psych::describe(as.data.frame(hourly_data[, .(total_consumption_kWh)]))
print(summary_stats)

daily_kWh <- hourly_data[, .(daily_kWh = sum(total_consumption_kWh, na.rm = TRUE)), by = date]
daily_kWh[, month_str := format(date, "%Y-%m")]
daily_kWh[, weekday_en := wday(date, label = TRUE, abbr = FALSE,
                               week_start = 1, locale = "C")]

# ------------------------------------------------
# 9. Exploration Plots
# ------------------------------------------------
p_hourly_ts <- ggplot(hourly_data, aes(interval, total_consumption_kWh)) +
  geom_line(color = "#0072B2") +
  labs(title = "Hourly Energy Consumption", x = "Datetime", y = "kWh") +
  theme_minimal()

p_time_series <- ggplot(daily_kWh, aes(date, daily_kWh)) +
  geom_line(color = "#E69F00") +
  labs(title = "Daily Energy Consumption", x = "Date", y = "kWh") +
  theme_minimal()

p_box_weekday <- ggplot(daily_kWh, aes(weekday_en, daily_kWh)) +
  geom_boxplot(fill = "#E69F00") +
  labs(title = "Daily Energy Consumption by Weekday") + theme_minimal()

p_box_month <- ggplot(daily_kWh, aes(month_str, daily_kWh)) +
  geom_boxplot(fill = "#56B4E9") +
  labs(title = "Daily Energy Consumption by Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_box_hour <- ggplot(hourly_data, aes(factor(hour), total_consumption_kWh)) +
  geom_boxplot(fill = "#009E73") +
  labs(title = "Hourly Energy Consumption by Hour of Day") + theme_minimal()

# ------------------------------------------------
# 10. Time series diagnostics
# ------------------------------------------------
energy_ts <- ts(hourly_data$total_consumption_kWh, frequency = 24)
par(mfrow = c(1, 2))
acf(energy_ts, main = "ACF: Hourly Energy", lag.max = 24*14)
pacf(energy_ts, main = "PACF: Hourly Energy", lag.max = 24*14)
par(mfrow = c(1, 1))

print(adf.test(energy_ts, alternative = "stationary"))

# ------------------------------------------------
# 11. Plot outputs
# ------------------------------------------------
print(p_hourly_ts)
print(p_time_series)
print(p_box_weekday)
print(p_box_month)
print(p_box_hour)

