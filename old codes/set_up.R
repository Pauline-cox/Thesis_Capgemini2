# ================= Install libraries ==========================================
initialize_environment <- function() {
  #  Set and lock Python version
  message("Configuring Python environment...")
  library(reticulate)
  use_python("C:/Users/pauli/AppData/Local/Programs/Python/Python310/python.exe", required = TRUE)
  
  # Install required R packages
  message("Installing/loading R packages...")
  packages <- c(
    "readxl", "data.table", "dplyr", "tidyr", "tibble", "stringr", "MASS", "tseries",
    "lubridate", "furrr", "future", "corrplot", "ggplot2", "forecast", "urca",
    "randomForest", "caret", "recipes", "Metrics" , "xgboost", "zoo", "purrr"
  )
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
  lapply(packages, library, character.only = TRUE)
  
  # Install required Python packages if needed
  message("Checking TensorFlow availability...")
  if (!py_module_available("tensorflow")) {
    message("📥 Installing Python packages via pip...")
    py_install(c(
      "tensorflow", "keras", "tensorflow-hub", "tensorflow-datasets",
      "scipy", "pandas", "h5py", "pillow", "requests"
    ))
  }
  
  # Load tensorflow and verify
  message("Loading TensorFlow...")
  library(tensorflow)
  library(keras)
  print(tf$constant("TensorFlow is ready!"))
}
