# run_forecasting.R
# -----------------------------------------------------------------------------
# Execute the energy forecasting pipeline with your prepared data
# -----------------------------------------------------------------------------

# Source the pipeline and your existing code
source("q.pipeline.R")  # Your existing pipeline
source("q.forecasting_pipeline.R")  # The forecasting pipeline

# Install required packages if needed
required_packages <- c("forecast", "keras", "tensorflow", "caret", "tidyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(forecast)
library(data.table)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# OPTION 1: Run with default horizons (24h, 1-week, 4-weeks)
# -----------------------------------------------------------------------------

# Run the full pipeline
results <- main_forecasting_pipeline(model_data)

# Access different components
detailed_results <- results$detailed_results
summary_table <- results$summary
performance_plot <- results$plot

# Save results
write.csv(summary_table, "forecasting_results_summary.csv", row.names = FALSE)
ggsave("model_performance_comparison.png", performance_plot, width = 12, height = 8, dpi = 300)

# -----------------------------------------------------------------------------
# OPTION 2: Custom configuration for specific horizons
# -----------------------------------------------------------------------------

# Define custom horizons if needed
custom_horizons <- list(
  h6 = 6,        # 6 hours
  h24 = 24,      # 24 hours
  h48 = 48,      # 2 days
  h168 = 168     # 1 week
)

# Run with custom horizons
custom_results <- run_forecasting_experiment(model_data, horizons = custom_horizons)
custom_summary <- analyze_results(custom_results)

# -----------------------------------------------------------------------------
# OPTION 3: Quick test with single horizon (for debugging/testing)
# -----------------------------------------------------------------------------

# Test with just 24-hour horizon
test_horizons <- list(h24 = 24)
test_results <- run_forecasting_experiment(model_data, horizons = test_horizons)

print("Forecasting pipeline completed!")
print("Check the generated CSV and PNG files for results.")

# -----------------------------------------------------------------------------
# OPTION 4: Detailed inspection of a single split
# -----------------------------------------------------------------------------

inspect_single_split <- function(model_data, horizon = 24) {
  # Take a simple train/test split for detailed inspection
  n_obs <- nrow(model_data)
  train_size <- floor(n_obs * 0.8)
  
  train_data <- model_data[1:train_size]
  test_data <- model_data[(train_size + 1):(train_size + horizon)]
  
  cat("Training data: ", nrow(train_data), " observations\n")
  cat("Test data: ", nrow(test_data), " observations\n")
  cat("Training period: ", as.character(train_data$interval[1]), " to ", 
      as.character(train_data$interval[nrow(train_data)]), "\n")
  cat("Test period: ", as.character(test_data$interval[1]), " to ", 
      as.character(test_data$interval[nrow(test_data)]), "\n")
  
  # Fit models and get predictions
  cat("\nFitting SARIMA...\n")
  sarima_model <- fit_sarima(train_data)
  sarima_pred <- forecast_sarima(sarima_model, horizon)
  
  cat("Fitting SARIMAX...\n")
  sarimax_model <- fit_sarimax(train_data)
  remaining_data <- model_data[(train_size + 1):nrow(model_data)]
  sarimax_pred <- forecast_sarimax(sarimax_model, remaining_data, horizon)
  
  # Create comparison plot
  actual_values <- test_data$total_consumption_kWh
  
  comparison_df <- data.frame(
    Hour = 1:length(actual_values),
    Actual = actual_values,
    SARIMA = sarima_pred[1:length(actual_values)],
    SARIMAX = sarimax_pred[1:length(actual_values)]
  )
  
  # Reshape for plotting
  library(tidyr)
  plot_data <- comparison_df %>%
    pivot_longer(cols = c(Actual, SARIMA, SARIMAX),
                 names_to = "Series", values_to = "Consumption")
  
  # Create plot
  p <- ggplot(plot_data, aes(x = Hour, y = Consumption, color = Series)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = paste("24-Hour Forecast Comparison"),
         subtitle = paste("Test period:", as.character(test_data$interval[1])),
         x = "Hour", y = "Energy Consumption (kWh)") +
    scale_color_manual(values = c("Actual" = "black", "SARIMA" = "blue", "SARIMAX" = "red"))
  
  print(p)
  
  # Calculate and print metrics
  sarima_metrics <- calculate_metrics(actual_values, sarima_pred[1:length(actual_values)])
  sarimax_metrics <- calculate_metrics(actual_values, sarimax_pred[1:length(actual_values)])
  
  cat("\nSARIMA Performance:\n")
  cat("MAE:", round(sarima_metrics$MAE, 2), "\n")
  cat("RMSE:", round(sarima_metrics$RMSE, 2), "\n")
  cat("MAPE:", round(sarima_metrics$MAPE, 2), "%\n")
  
  cat("\nSARIMAX Performance:\n")
  cat("MAE:", round(sarimax_metrics$MAE, 2), "\n")
  cat("RMSE:", round(sarimax_metrics$RMSE, 2), "\n")
  cat("MAPE:", round(sarimax_metrics$MAPE, 2), "%\n")
  
  return(list(
    comparison_data = comparison_df,
    plot = p,
    sarima_metrics = sarima_metrics,
    sarimax_metrics = sarimax_metrics
  ))
}

# Run single split inspection (uncomment to run)
# single_split_results <- inspect_single_split(model_data)

# Print session info for reproducibility
cat("\n=== Session Info ===\n")
print(sessionInfo())