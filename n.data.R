# R/20_data.R
# -----------------------------------------------------------------------------
# Data IO and preparation wrappers.
# Assumes you have your own loading & feature scripts already.
# -----------------------------------------------------------------------------

# source("load_data_new.R")   # <- your function load_and_prepare_data()
# source("features.R")        # <- your function prepare_features()

prepare_data <- function(data, horizon) {
  dt <- copy(as.data.table(data))
  dt <- na.omit(dt)
  
  # Shift the target forward by 'horizon' so predictors are strictly lagged
  dt[, target := shift(total_consumption_kWh, -horizon)]
  dt <- na.omit(dt)
  dt
}

split_data <- function(data, split_ratio = 0.8) {
  n <- nrow(data)
  idx <- floor(n * split_ratio)
  list(train = data[1:idx], test = data[(idx + 1):n])
}
