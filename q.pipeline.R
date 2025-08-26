# q.pipeline.R
# -----------------------------------------------------------------------------
# Full pipeline: load, prep, select variables, train models, evaluate.
# -----------------------------------------------------------------------------

# =============== Set up and prepare data =====================================

# --- Set up environment and load functions ---
source("q.initialize.R"); initialize_environment()
source("load_data.R")
source("features.R")
source("q.prep_data.R")

# --- Load data and prepare features ---
raw_data   <- load_and_prepare_data()
model_data <- prepare_features(raw_data)

# --- Select smaller part for quicker computation in testing ---

raw_data_copy <- raw_data
raw_data <- raw_data_copy
# Filter raw_data to desired period
raw_data <- raw_data[
  interval >= as.POSIXct("2023-05-01 00:00:00", tz = "UTC") &
    interval <  as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
]
# n = total rows
n <- nrow(model_data)

# second half
model_data_half2 <- model_data[(floor(n/2) + 1):n]

# check
print(nrow(model_data_half2))   # should be about half
head(model_data_half2)


model_data_small <- head(model_data, n = nrow(model_data) / 10)

# --- Forecast horizon and split data ---
horizon <- 24
data <- prepare_target(model_data, horizon)
sets <- split_data(data)
train_data <- sets$train
test_data  <- sets$test
actual     <- test_data$target

# =============== xxx ============================================



