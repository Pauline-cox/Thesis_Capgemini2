load_and_prepare_data <- function() {
  
  # ---------------- Load and filter sensor data ------------------------------
  sensor_data_path <- "Data/Sensor data/NL-UTC-UTRECHTLR/"
  active_sensors_path <- "Data/Sensor data/selection_NL-UTC"
  
  sensor_files <- list.files(sensor_data_path, pattern = "\\.csv$", full.names = TRUE)
  active_files <- list.files(active_sensors_path, pattern = "\\.csv$", full.names = TRUE)
  
  extract_ym_active <- function(filename) {
    match <- str_match(basename(filename), "NL-UTC_(\\d{6})\\d*\\.csv$")
    if (is.na(match[1,2])) return(NA_character_)
    return(match[1, 2])
  }
  
  extract_ym_sensor <- function(filename) {
    match <- str_match(basename(filename), "sensorData_(\\d{6})\\.csv$")
    if (is.na(match[1,2])) return(NA_character_)
    return(match[1, 2])
  }
  
  ym_keys <- sapply(active_files, extract_ym_active)
  valid_indices <- !is.na(ym_keys)
  active_by_month <- split(active_files[valid_indices], ym_keys[valid_indices])
  
  filtered_sensor_files <- sensor_files[sapply(sensor_files, function(f) {
    ym <- extract_ym_sensor(f)
    ym %in% names(active_by_month) && substr(ym, 1, 4) %in% c("2023", "2024")
  })]
  
  all_data <- rbindlist(lapply(filtered_sensor_files, function(sensor_file) {
    ym <- extract_ym_sensor(sensor_file)
    active_files_month <- active_by_month[[ym]]
    if (is.null(active_files_month)) return(NULL)
    
    active_sensors <- unique(rbindlist(lapply(active_files_month, function(f) {
      dt <- tryCatch(suppressWarnings(fread(f, fill = TRUE)), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      
      char_cols <- dt[, which(sapply(.SD, is.character)), .SDcols = names(dt)]
      if (length(char_cols) == 0) return(NULL)
      
      eui_pattern <- "eui-[0-9a-f]+"
      euis <- unlist(lapply(dt[, ..char_cols], function(col) {
        unique(str_extract(col, eui_pattern))
      }))
      
      data.table(sensor = na.omit(euis))
    }), fill = TRUE)$sensor)
    
    sensor_data <- fread(sensor_file, na.strings = "")
    sensor_data[deviceName %in% active_sensors]
  }), fill = TRUE)
  
  sensor_data <- all_data
  
  # ---------------- Process occupancy data -----------------------------------
  process_occupancy <- function(sensor_data) {
    desk_data <- sensor_data[
      application %in% c("officesense_desk", "officesense_desk_EU868", "officesense_desk_EU868_gk"),
      .(time, deviceName, occupied)
    ]
    
    desk_data[occupied == 2, occupied := 1]
    desk_data <- desk_data[occupied %in% c(0, 1)]
    desk_data[, time := as.POSIXct(time, tz = "UTC")]
    
    # Remove heartbeat signals
    setorder(desk_data, deviceName, time)
    desk_data[, occupied_lag := shift(occupied), by = deviceName]
    desk_data <- desk_data[is.na(occupied_lag) | occupied != occupied_lag]
    desk_data[, occupied_lag := NULL]
    
    # Add zero at end-of-day if still occupied
    desk_data[, date := as.Date(time)]
    setorder(desk_data, deviceName, time)
    last_obs <- desk_data[, .SD[.N], by = .(deviceName, date)]
    still_occupied <- last_obs[occupied == 1]
    fake_end_obs <- still_occupied[, .(
      time = as.POSIXct(paste0(date, " 23:59:59"), tz = "UTC"),
      deviceName,
      occupied = 0
    )]
    fake_end_obs[, date := as.Date(time)]
    desk_data <- rbindlist(list(desk_data, fake_end_obs), use.names = TRUE)
    setorder(desk_data, deviceName, time)
    
    # Build sessions
    sessions <- desk_data[, .(
      start_time = time[occupied == 1],
      end_time   = shift(time, type = "lead")[occupied == 1]
    ), by = deviceName]
    sessions <- sessions[!is.na(end_time) & end_time > start_time]
    
    expand_hour <- function(start, end) {
      data.table(interval = seq(
        floor_date(start, "hour"),
        ceiling_date(end, "hour") - minutes(1),
        by = "hour"
      ))
    }
    
    
    occupancy_intervals <- sessions[
      , expand_hour(start_time, end_time), 
      by = .(deviceName, start_time, end_time)
    ]
    
    occupancy_intervals[, occupied := 1]
    
    # Optional: fill missing combinations
    # Optional: complete hourly grid
    all_times <- seq(
      floor_date(min(desk_data$time), "day"),
      ceiling_date(max(desk_data$time), "day") - hours(1),
      by = "hour"
    )
    
    all_combinations <- CJ(deviceName = unique(desk_data$deviceName), interval = all_times)
    result <- merge(all_combinations, occupancy_intervals, by = c("deviceName", "interval"), all.x = TRUE)
    result[is.na(occupied), occupied := 0]
    
    
    all_combinations <- CJ(deviceName = unique(desk_data$deviceName), interval = all_times)
    result <- merge(all_combinations, occupancy_intervals, by = c("deviceName", "interval"), all.x = TRUE)
    result[is.na(occupied), occupied := 0]
    
    # Total per 15-minute interval
    total_occupancy <- result[, .(total_occupancy = sum(occupied)), by = interval]
    
    return(total_occupancy)
  }
  
  occupancy_15min <- process_occupancy(sensor_data)
  
  # ---------------- Comfort data (averaged hourly) ----------------------------
  comfort <- sensor_data[
    application %in% c("officesense_comfort_EU868", "officesense_comfort_fw2"),
    .(time, tempC, humidity, co2, sound, lux)
  ]
  comfort[, time := as.POSIXct(time)]
  comfort[, interval := floor_date(time, "hour")]
  comfort_hourly <- comfort[, .(
    tempC = mean(tempC, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    co2 = mean(co2, na.rm = TRUE),
    sound = mean(sound, na.rm = TRUE),
    lux = mean(lux, na.rm = TRUE)
  ), by = interval]
  
  # ---------------- Energy data (summed hourly) -------------------------------
  energy_2023 <- read_excel("Data/Eneco/Kantoorgebouw K20016264_2023_871687460008204791_E (1).xlsx")
  energy_2024 <- read_excel("Data/Eneco/Kantoorgebouw - K20016264_2024_871687460008204791_E (1).xlsx")
  
  energy_data <- rbindlist(list(energy_2023, energy_2024), fill = TRUE) %>%
    mutate(
      interval = floor_date(`Start Datum`, "hour"),
      total_consumption_kWh = `Levering Dal` + `Levering Piek`
    ) %>%
    group_by(interval) %>%
    summarise(total_consumption_kWh = sum(total_consumption_kWh, na.rm = TRUE), .groups = "drop") %>%
    as.data.table()
  
  # ---------------- KNMI data (hourly) ----------------------------------------
  KNMI_raw <- readLines("Data/KNMI.txt")
  KNMI_clean <- KNMI_raw[!startsWith(KNMI_raw, "#")]
  KNMI_data <- fread(text = KNMI_clean, header = FALSE)
  
  setnames(KNMI_data, c("STN", "YYYYMMDD", "HH", "DD", "FH", "FF", "FX", "T", "T10N", "TD",
                        "SQ", "Q", "DR", "RH", "P", "VV", "N", "U", "WW", "IX", "M", "R", "S", "O", "Y"))
  
  KNMI_data <- KNMI_data[, .(
    YYYYMMDD, HH,
    temperature = T,
    wind_speed = FH,
    sunshine_minutes = SQ,
    global_radiation = Q,
    humidity_percent = U,
    fog = M,
    rain = R,
    snow = S,
    thunder = O,
    ice = Y
    
    # optional:
    # dew_point = TD,
    # precipitation_mm_0.1 = RH,
    # precip_duration_0.1h = DR 
  )]
  
  KNMI_data[, interval := as.POSIXct(sprintf("%s %02d:00:00", YYYYMMDD, HH), format = "%Y%m%d %H:%M:%S")]
  KNMI_data[, c("YYYYMMDD", "HH") := NULL]
  
  # ---------------- Merge all data together -----------------------------------
  raw_data <- Reduce(function(x, y) merge(x, y, by = "interval", all = TRUE),
                     list(occupancy_15min, comfort_hourly, energy_data, KNMI_data))
  
  # ---------------- Fill missing values ---------------------------------------
  setorder(raw_data, interval)
  cols_to_fill <- setdiff(names(raw_data), "interval")
  for (col in cols_to_fill) {
    raw_data[[col]] <- na.locf(raw_data[[col]], na.rm = FALSE)
    raw_data[[col]] <- na.locf(raw_data[[col]], fromLast = TRUE, na.rm = FALSE)
  }
  
  return(raw_data)
}
