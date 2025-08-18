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
model_data_small <- head(model_data, n = nrow(model_data) / 10)

# --- Forecast horizon and split data ---
horizon <- 1
data <- prepare_target(model_data_small, horizon)
sets <- split_data(data)
train_data <- sets$train
test_data  <- sets$test
actual     <- test_data$target

# =============== xxx ============================================



