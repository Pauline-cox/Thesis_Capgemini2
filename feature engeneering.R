prepare_features <- function(data,
                             lags  = c(24, 48, 72, 168, 336, 504, 720),
                             rolls = c(24, 168, 720),
                             add_fourier = TRUE, K = 3) {
  if (!data.table::is.data.table(data)) data <- as.data.table(data)
  stopifnot("total_consumption_kWh" %in% names(data),
            "interval" %in% names(data))
  
  engineered_data <- copy(data)
  
  # ---------------- Time-based features ----------------
  engineered_data[, date := as.Date(interval)]
  engineered_data[, hour := lubridate::hour(interval)]
  old_loc <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_loc), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  
  engineered_data[, weekday := factor(
    weekdays(date),
    levels = c("Monday","Tuesday","Wednesday","Thursday",
               "Friday","Saturday","Sunday"),
    ordered = TRUE)]
  
  engineered_data[, office_hours := as.integer(hour >= 9 & hour <= 17)]
  engineered_data[, is_weekend := as.integer(weekdays(date) %in% c("Saturday","Sunday"))]
  
  engineered_data[, hour_sin := sin(2*pi*hour/24)]
  engineered_data[, hour_cos := cos(2*pi*hour/24)]
  
  engineered_data[, month := factor(lubridate::month(date, label=TRUE, abbr=FALSE),
                                    levels=month.name, ordered=TRUE)]
  engineered_data[, year := as.factor(lubridate::year(date))]
  
  # ---------------- Holidays ----------------
  nl_holidays <- as.Date(c(
    "2023-01-01","2023-04-07","2023-04-09","2023-04-10","2023-04-27",
    "2023-05-05","2023-05-18","2023-05-28","2023-05-29","2023-12-25","2023-12-26",
    "2024-01-01","2024-03-29","2024-03-31","2024-04-01","2024-04-27",
    "2024-05-05","2024-05-09","2024-05-19","2024-05-20","2024-12-25","2024-12-26"
  ))
  school_holidays <- as.Date(c(
    seq(as.Date("2023-04-29"), as.Date("2023-05-07"), by="day"),
    seq(as.Date("2024-04-27"), as.Date("2024-05-05"), by="day"),
    seq(as.Date("2023-07-08"), as.Date("2023-08-20"), by="day"),
    seq(as.Date("2024-07-13"), as.Date("2024-08-25"), by="day"),
    seq(as.Date("2023-12-23"), as.Date("2024-01-07"), by="day"),
    seq(as.Date("2024-12-21"), as.Date("2025-01-05"), by="day"),
    seq(as.Date("2024-02-17"), as.Date("2024-02-25"), by="day")
  ))
  mandatory_days_off <- as.Date(c("2023-04-28","2023-05-19","2023-12-27",
                                  "2024-05-10","2024-12-27"))
  all_holidays <- unique(c(nl_holidays, school_holidays, mandatory_days_off))
  engineered_data[, holiday := as.integer(date %in% all_holidays)]
  
  # ---------------- Lag features ----------------
  lag_cols <- paste0("lag_", lags)
  for (i in seq_along(lags)) {
    set(engineered_data, j=lag_cols[i],
        value=shift(engineered_data$total_consumption_kWh, lags[i]))
  }
  
  # ---------------- Rolling stats ----------------
  roll_cols <- paste0("rollmean_", rolls)
  for (i in seq_along(rolls)) {
    set(engineered_data, j=roll_cols[i],
        value=zoo::rollmeanr(engineered_data$total_consumption_kWh, rolls[i], fill=NA))
  }
  roll_sd_cols <- paste0("rollsd_", rolls)
  for (i in seq_along(rolls)) {
    set(engineered_data, j=roll_sd_cols[i],
        value=zoo::rollapplyr(engineered_data$total_consumption_kWh, rolls[i], sd, fill=NA))
  }
  # Ratios (load deviations)
  for (r in rolls) {
    dev_col <- paste0("load_dev_", r)
    set(engineered_data, j=dev_col,
        value=engineered_data$total_consumption_kWh / (engineered_data[[paste0("rollmean_", r)]] + 1e-6))
  }
  
  # ---------------- Weather interactions ----------------
  if (all(c("tempC","total_occupancy") %in% names(engineered_data)))
    engineered_data[, temp_occ := tempC * total_occupancy]
  if ("tempC" %in% names(engineered_data))
    engineered_data[, temp_office := tempC * office_hours]
  if ("global_radiation" %in% names(engineered_data))
    engineered_data[, rad_weekend := global_radiation * is_weekend]
  
  # ---------------- Weather deltas ----------------
  if ("tempC" %in% names(engineered_data))
    engineered_data[, delta_temp := tempC - shift(tempC, 24)]
  if ("humidity" %in% names(engineered_data))
    engineered_data[, delta_humidity := humidity - shift(humidity, 24)]
  if ("wind_speed" %in% names(engineered_data))
    engineered_data[, delta_wind := wind_speed - shift(wind_speed, 24)]
  
  # ---------------- Fourier terms (weekly) ----------------
  if (add_fourier) {
    ts_data <- ts(engineered_data$total_consumption_kWh, frequency=168)  # weekly seasonality
    f_terms <- forecast::fourier(ts_data, K=K)
    engineered_data <- cbind(engineered_data, as.data.table(f_terms))
  }
  
  # ---------------- Cleanup ----------------
  new_cols <- c(lag_cols, roll_cols, roll_sd_cols,
                paste0("load_dev_", rolls),
                "temp_occ","temp_office","rad_weekend",
                "delta_temp","delta_humidity","delta_wind")
  if (add_fourier) new_cols <- c(new_cols, colnames(f_terms))
  
  na_mask <- complete.cases(engineered_data[, ..new_cols])
  cleaned_data <- engineered_data[na_mask]
  
  setattr(cleaned_data, "lags_used", lags)
  setattr(cleaned_data, "rolls_used", rolls)
  setattr(cleaned_data, "rows_removed", sum(!na_mask))
  
  return(cleaned_data)
}

model_data <- prepare_features(processed_data)
