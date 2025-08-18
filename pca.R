perform_pca_reduction <- function(data, response_var, variance_threshold = NULL, n_components = NULL) {
  if (!(response_var %in% names(data))) {
    stop(paste0("Column '", response_var, "' is missing in the input data."))
  }
  
  response <- data[[response_var]]
  interval <- if ("interval" %in% names(data)) data$interval else NULL
  
  predictor_cols <- setdiff(names(data), c("interval", response_var))
  numeric_cols <- names(data[, ..predictor_cols])[sapply(data[, ..predictor_cols], is.numeric)]
  predictors_numeric <- data[, ..numeric_cols]
  
  if (nrow(predictors_numeric) != length(response)) {
    stop("Mismatch in number of rows between predictors and response.")
  }
  
  predictors_scaled <- scale(predictors_numeric)
  pca <- prcomp(predictors_scaled, center = TRUE, scale. = TRUE)
  
  # Select number of PCs
  if (is.null(n_components)) {
    explained_var <- summary(pca)$importance["Cumulative Proportion", ]
    n_components <- which(explained_var >= variance_threshold)[1]
  }
  
  pca_data <- as.data.table(pca$x[, 1:n_components, drop = FALSE])
  set(pca_data, j = response_var, value = response)
  if (!is.null(interval)) set(pca_data, j = "interval", value = interval)
  
  return(list(
    pca_data = pca_data,
    pca_model = pca,
    selected_components = n_components,
    explained_variance = summary(pca)$importance["Cumulative Proportion", 1:n_components]
  ))
}


# Assume `data` has been processed with prepare_data()
pca_result <- perform_pca_reduction(data, response_var = "target", variance_threshold = 0.95)
data <- pca_result$pca_data  # Replace your full dataset with principal components

get_model_formula <- function() {
  pcs <- grep("^PC", names(data), value = TRUE)
  as.formula(paste("target ~", paste(pcs, collapse = " + ")))
}

