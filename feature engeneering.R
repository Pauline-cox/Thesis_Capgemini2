prepare_features <- function(data, lags = c(24, 48, 72, 168, 336, 504), rolls = c(24, 168)) {
  # Input validation
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  if (!"total_consumption_kWh" %in% names(data)) {
    stop("Data must contain 'total_consumption_kWh' column")
  }
  
  if (!"interval" %in% names(data)) {
    stop("Data must contain 'interval' column")
  }
  
  engineered_data <- data.table::copy(data)
  
  # ---------------- Time-based features --------------------------------------
  engineered_data[, date := as.Date(interval)]
  engineered_data[, hour := lubridate::hour(interval)]
  
  # Store original locale and restore on exit (safer approach)
  old_loc <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_loc), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  
  # Weekday as ordered factor
  engineered_data[, weekday := factor(
    weekdays(date),
    levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
    ordered = TRUE
  )]
  
  # Weekend + office hours
  engineered_data[, office_hours := as.integer(hour >= 9 & hour <= 17)]
  engineered_data[, is_weekend := as.integer(weekdays(date) %in% c("Saturday", "Sunday"))]
  
  # Cyclical encoding for hour
  engineered_data[, hour_sin := sin(2 * pi * hour / 24)]
  engineered_data[, hour_cos := cos(2 * pi * hour / 24)]
  
  # Month as ordered factor
  engineered_data[, month := factor(
    lubridate::month(date, label = TRUE, abbr = FALSE),
    levels = c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December"),
    ordered = TRUE
  )]
  
  # Year as factor
  engineered_data[, year := as.factor(lubridate::year(date))]
  
  # ---------------- Dutch holidays (parameterized for flexibility) -----------
  nl_holidays <- as.Date(c(
    "2023-01-01", "2023-04-07", "2023-04-09", "2023-04-10", "2023-04-27",
    "2023-05-05", "2023-05-18", "2023-05-28", "2023-05-29", "2023-12-25", "2023-12-26",
    "2024-01-01", "2024-03-29", "2024-03-31", "2024-04-01", "2024-04-27",
    "2024-05-05", "2024-05-09", "2024-05-19", "2024-05-20", "2024-12-25", "2024-12-26"
  ))
  
  school_holidays <- as.Date(c(
    seq(as.Date("2023-04-29"), as.Date("2023-05-07"), by = "day"),
    seq(as.Date("2024-04-27"), as.Date("2024-05-05"), by = "day"),
    seq(as.Date("2023-07-08"), as.Date("2023-08-20"), by = "day"),
    seq(as.Date("2024-07-13"), as.Date("2024-08-25"), by = "day"),
    seq(as.Date("2023-12-23"), as.Date("2024-01-07"), by = "day"),
    seq(as.Date("2024-12-21"), as.Date("2025-01-05"), by = "day"),
    seq(as.Date("2024-02-17"), as.Date("2024-02-25"), by = "day")
  ))
  
  mandatory_days_off <- as.Date(c(
    "2023-04-28", "2023-05-19", "2023-12-27",
    "2024-05-10", "2024-12-27"
  ))
  
  all_holidays <- unique(c(nl_holidays, school_holidays, mandatory_days_off))
  engineered_data[, holiday := as.integer(date %in% all_holidays)]
  
  # ---------------- Lag features ---------------------------------------------
  # Use set for efficiency with large datasets
  lag_cols <- paste0("lag_", lags)
  for (i in seq_along(lags)) {
    data.table::set(engineered_data, j = lag_cols[i], 
                    value = data.table::shift(engineered_data$total_consumption_kWh, lags[i]))
  }
  
  # ---------------- Rolling statistics ---------------------------------------
  roll_cols <- paste0("rollmean_", rolls)
  for (i in seq_along(rolls)) {
    data.table::set(engineered_data, j = roll_cols[i],
                    value = zoo::rollmeanr(engineered_data$total_consumption_kWh, rolls[i], fill = NA))
  }
  
  # Optional: Add rolling standard deviations for volatility
  roll_sd_cols <- paste0("rollsd_", rolls)
  for (i in seq_along(rolls)) {
    data.table::set(engineered_data, j = roll_sd_cols[i],
                    value = zoo::rollapplyr(engineered_data$total_consumption_kWh, rolls[i], sd, fill = NA))
  }
  
  # ---------------- Final cleanup --------------------------------------------
  # Keep all columns for cleaning
  all_numeric_cols <- c(lag_cols, roll_cols, roll_sd_cols)
  
  # Remove rows with NA in any of the generated features
  na_mask <- complete.cases(engineered_data[, ..all_numeric_cols])
  cleaned_data <- engineered_data[na_mask]
  
  # Add informative attributes
  data.table::setattr(cleaned_data, "lags_used", lags)
  data.table::setattr(cleaned_data, "rolls_used", rolls)
  data.table::setattr(cleaned_data, "rows_removed", sum(!na_mask))
  
  return(cleaned_data)
}

model_data <- prepare_features(processed_data)
