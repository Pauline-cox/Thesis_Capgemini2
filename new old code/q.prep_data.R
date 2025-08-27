# q.prep_data.R
# -----------------------------------------------------------------------------
# Functions that prepare the target variable and split train/test set
# -----------------------------------------------------------------------------

prepare_target <- function(data, horizon) {
  dt <- copy(as.data.table(data))
  dt <- na.omit(dt)
  # Shift the target forward by 'horizon' so predictors are strictly lagged
  dt[, target := shift(total_consumption_kWh, -horizon)]
  dt <- na.omit(dt)
  dt
}

# Split ratio in train and test set
split_data <- function(data, split_ratio = 0.9) {
  n <- nrow(data)
  idx <- floor(n * split_ratio)
  list(train = data[1:idx], test = data[(idx + 1):n])
}
