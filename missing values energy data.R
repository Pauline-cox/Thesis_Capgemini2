library(dplyr)
library(lubridate)

# Function to check for missing intervals
check_missing_intervals <- function(df, start_col = "Start Datum", end_col = "Eind Datum") {
  start_times <- df[[start_col]]
  
  # Build complete sequence from min to max timestamp
  full_seq <- seq(from = min(start_times),
                  to   = max(start_times),
                  by   = "15 min")
  
  # Find which expected timestamps are missing
  missing <- setdiff(full_seq, start_times)
  
  return(missing)
}

missing_2023 <- check_missing_intervals(energy_2023)
missing_2024 <- check_missing_intervals(energy_2024)

length(missing_2023)
length(missing_2024)

print(missing_2023)
print(missing_2024)
