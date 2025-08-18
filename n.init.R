# R/00_init.R
# -----------------------------------------------------------------------------
# Initializes environment, loads libraries, sets seeds.
# -----------------------------------------------------------------------------

# If you have your own env in set_up.R (e.g., reticulate / TensorFlow), source it here.
source("set_up.R"); initialize_environment()

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(caret)
  library(forecast)
  library(ranger)
  library(xgboost)
  library(SHAPforxgboost)
  library(recipes)
  library(keras)
})

set.seed(1234)                     # R seed
tensorflow::set_random_seed(1234)  # TF/Keras seed
