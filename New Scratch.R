
# =============================================================================
# Time Series Diagnostics for SARIMA Modeling
# - Performs ADF test for stationarity
# - Plots ACF and PACF to assist in identifying p, d, q, P, D, Q
# =============================================================================
diagnose_time_series <- function(ts_data, max_lag = 48, seasonal_period = 24*7) {
  cat("=== Augmented Dickey-Fuller (ADF) Test ===\n")
  adf_result <- adf.test(ts_data, alternative = "stationary")
  print(adf_result)
  
  cat("\n=== ACF Plot ===\n")
  acf(ts_data, lag.max = max_lag, main = "ACF: Autocorrelation Function")
  
  cat("\n=== PACF Plot ===\n")
  pacf(ts_data, lag.max = max_lag, main = "PACF: Partial Autocorrelation Function")
}
# --- Diagnostics before fitting SARIMA ---
ts_train <- ts(train$target, frequency = 24 * 7)
diagnose_time_series(ts_train)


# =============================================================================
# Extract and plot feature importance for RF
# =============================================================================

# Extract importanceWi
importance_df <- data.frame(
  Feature = rownames(varImp(rf_model)$importance),
  Importance = varImp(rf_model)$importance$Overall
)


# Use the first numeric column for plotting
importance_metric <- names(importance_df)[sapply(importance_df, is.numeric)][1]

ggplot(importance_df, aes(x = reorder(Feature, .data[[importance_metric]]), y = .data[[importance_metric]])) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    x = "Feature",
    y = importance_metric
  ) +
  theme_minimal()


# =============================================================================
# Make correlation plot
# =============================================================================

# Convert to data.frame if needed
df <- as.data.frame(model_data)

# Select only numeric columns
numeric_cols <- sapply(df, is.numeric)
df_numeric <- df[, numeric_cols]

# Identify and remove constant columns
zero_var_cols <- names(df_numeric)[sapply(df_numeric, function(x) sd(x, na.rm = TRUE) == 0)]
if (length(zero_var_cols) > 0) {
  cat("Removed constant variables:\n")
  print(zero_var_cols)
}
df_numeric <- df_numeric[, !(names(df_numeric) %in% zero_var_cols)]

# Compute correlation matrix
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Extract and print correlations with total_consumption_kWh
cor_target <- cor_matrix["total_consumption_kWh", ]
cor_df <- data.frame(
  variable = names(cor_target),
  correlation = round(cor_target, 3)
)

# Sort by absolute correlation
cor_df <- cor_df[order(-abs(cor_df$correlation)), ]

# Print the result
print(cor_df)

# =============================================================================
# SHAP values
# =============================================================================
library(SHAPforxgboost)

# Train your model as usual
x <- model.matrix(get_model_formula(), train_data)[, -1]
y <- train_data$target
xgb_model <- xgboost(data = x, label = y, nrounds = 100, objective = "reg:squarederror")

# Compute SHAP values
shap_values <- shap.values(xgb_model = xgb_model, X_train = x)

# Plot summary
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = x)
shap.plot.summary(shap_long)
print(shap.plot.summary(shap_long))

library(randomForest)
library(iml)

rf_model <- randomForest(get_model_formula(), data = train_data, ntree = 300)

# Prepare data and predictor object
X <- train_data[, all.vars(get_model_formula())[-1], with = FALSE]
predictor <- Predictor$new(rf_model, data = X, y = train_data$target)

# Calculate SHAP values for a few instances
shap <- Shapley$new(predictor, x.interest = X[1, ])
plot(shap)
print(plot(shap))
