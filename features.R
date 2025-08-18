prepare_features <- function(data, lags = c(24, 48, 72, 168, 336, 504), rolls = c(24, 168)) {
  engineered_data <- copy(data)  # Avoid modifying original
  engineered_data[, date := as.Date(interval)]  # Add date column early
  
  # ---------------- Time-based features --------------------------------------
  engineered_data[, hour := hour(interval)]
  
  weekdays_list <- c("maandag", "dinsdag", "woensdag", "donderdag", "vrijdag", "zaterdag")
  weekday_names <- weekdays(engineered_data$date)
  
  for (day in weekdays_list) {
    col_name <- paste0("is_", tolower(day))
    engineered_data[, (col_name) := as.integer(tolower(weekday_names) == day)]
  }
  
  engineered_data[, office_hours := as.integer(hour >= 9 & hour <= 17)]
  engineered_data[, is_weekend := as.integer(tolower(weekdays(date)) %in% c("zaterdag", "zondag"))]
  engineered_data[, weekday := factor(weekdays(as.Date(interval)))]
  
  engineered_data[, hour_sin := sin(2 * pi * hour / 24)]
  engineered_data[, hour_cos := cos(2 * pi * hour / 24)]
  
  # ---------------- Dutch holidays -------------------------------------------
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
  
  # ---------------- Impute sequences of >6 consecutive zeros -------------------
  engineered_data[, zero_flag := as.integer(total_consumption_kWh == 0)]
  engineered_data[, zero_run := rleid(zero_flag)]
  
  # Identify runs of zeros longer than 6
  zero_runs <- engineered_data[zero_flag == 1, .N, by = zero_run][N > 6]$zero_run
  
  # Impute each long zero run
  for (zr in zero_runs) {
    rows_to_impute <- engineered_data[zero_run == zr & zero_flag == 1]
    
    for (i in seq_len(nrow(rows_to_impute))) {
      hour_val <- rows_to_impute$hour[i]
      date_val <- rows_to_impute$date[i]
      weekday_val <- weekdays(date_val)
      
      replacement <- median(engineered_data[
        weekdays(date) == weekday_val & hour == hour_val & total_consumption_kWh > 0,
        total_consumption_kWh
      ], na.rm = TRUE)
      
      if (!is.na(replacement)) {
        engineered_data[zero_run == zr & hour == hour_val & date == date_val, total_consumption_kWh := replacement]
      }
    }
  }
  
  # Cleanup helper columns
  engineered_data[, ':='(zero_flag = NULL, zero_run = NULL)]
  
  # ---------------- Lag features ---------------------------------------------
  for (lag in lags) {
    col_name <- paste0("lag_", lag)
    engineered_data[, (col_name) := shift(total_consumption_kWh, lag)]
  }
  
  # ---------------- Rolling means --------------------------------------------
  for (roll in rolls) {
    col_name <- paste0("rollmean_", roll)
    engineered_data[, (col_name) := zoo::rollmeanr(total_consumption_kWh, roll, fill = NA)]
  }
  
  # ---------------- Final cleanup --------------------------------------------
  lag_cols <- paste0("lag_", lags)
  roll_cols <- paste0("rollmean_", rolls)
  clean_cols <- c(lag_cols, roll_cols)
  
  cleaned_data <- engineered_data[complete.cases(engineered_data[, ..clean_cols])]
  
  # ---------------- Normalize numeric columns (0 to 1) ------------------------
  numeric_cols <- names(which(sapply(cleaned_data, is.numeric)))
  
  # Optional: exclude 'total_consumption_kWh', date columns, interval if needed
  exclude_cols <- c("total_consumption_kWh", "YYYYMMDD", "HH", "interval")
  normalize_cols <- setdiff(numeric_cols, exclude_cols)
  
  for (col in normalize_cols) {
    min_val <- min(cleaned_data[[col]], na.rm = TRUE)
    max_val <- max(cleaned_data[[col]], na.rm = TRUE)
    
    if (max_val != min_val) {
      cleaned_data[[col]] <- (cleaned_data[[col]] - min_val) / (max_val - min_val)
    } else {
      cleaned_data[[col]] <- 0  # If constant column, set to 0
    }
  }
  
  return(cleaned_data)
}
