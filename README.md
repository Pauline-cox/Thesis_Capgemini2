

> cat("Grid Search Complete!\n")
Grid Search Complete!
> cat("Total time:", round(total_time, 2), "minutes\n\n")
Total time: 1.05 minutes

> # Display results sorted by AIC
> results_clean <- results[results$convergence == TRUE, ]
> results_sorted <- results_clean[order(results_clean$AIC), ]
> cat("=== GRID SEARCH RESULTS (sorted by AIC) ===\n")
=== GRID SEARCH RESULTS (sorted by AIC) ===
> print(results_sorted[, c("p", "d", "q", "P", "D", "Q", "AIC", "AICc", "BIC", "loglik")])
   p d q P D Q      AIC     AICc      BIC    loglik
3  2 0 2 1 1 2 156631.4 156631.4 156692.5 -78307.69
23 3 1 2 1 1 2 156997.2 156997.3 157066.0 -78489.62
18 4 1 2 2 1 1 157110.3 157110.3 157186.7 -78545.14
10 4 1 1 1 1 2 157123.5 157123.5 157192.3 -78552.76
12 4 1 2 0 1 1 157132.0 157132.0 157193.1 -78558.02
24 4 1 2 1 1 2 157136.7 157136.7 157213.1 -78558.34
4  2 1 2 1 1 2 157427.3 157427.3 157488.4 -78705.65
22 2 1 2 1 1 2 157427.3 157427.3 157488.4 -78705.65
2  2 1 1 2 1 1 157464.5 157464.5 157518.0 -78725.25
20 3 0 0 1 1 2 158232.7 158232.7 158286.2 -79109.37
7  3 0 2 2 1 1 158233.1 158233.1 158301.8 -79107.53
21 4 0 0 1 1 2 158234.2 158234.2 158295.3 -79109.12
11 4 0 2 0 1 1 158234.5 158234.5 158295.6 -79109.23
9  4 0 1 1 1 2 158236.1 158236.1 158304.9 -79109.06
5  3 0 1 0 1 2 158250.3 158250.3 158303.8 -79118.16
19 2 0 0 1 1 2 158531.9 158531.9 158577.7 -79259.96
14 2 1 2 2 1 1 158648.2 158648.2 158709.3 -79316.09
13 2 1 0 2 1 1 158649.7 158649.7 158695.5 -79318.86
1  2 0 1 2 1 1 158652.3 158652.3 158705.8 -79319.15
8  3 1 2 2 1 1 158658.1 158658.1 158726.8 -79320.05
16 3 1 2 2 1 1 158658.1 158658.1 158726.8 -79320.05
6  3 1 1 0 1 2 158699.1 158699.1 158752.6 -79342.56
15 3 1 0 2 1 1 160410.2 160410.2 160463.7 -80198.11
17 4 1 0 2 1 1 160411.4 160411.4 160472.5 -80197.70
> # Fit and analyze the best model
> if (nrow(results_sorted) > 0) {
+   cat("\n=== BEST MODEL ANALYSIS ===\n")
+   
+   best_params <- results_sorted[1, ]
+   cat("Best model: SARIMA(", best_params$p, ",", best_params$d, ",", best_params$q, 
+       ")(", best_params$P, ",", best_params$D, ",", best_params$Q, ")[168]\n\n")
+   
+   # Refit the best model for detailed analysis
+   best_model <- Arima(y_train,
+                       order = c(best_params$p, best_params$d, best_params$q),
+                       seasonal = list(order = c(best_params$P, best_params$D, best_params$Q), 
+                                       period = 168),
+                       # xreg = xreg_train,  # Uncomment when you have exogenous variables
+                       method = "ML")
+   
+   # Print model summary
+   cat("Model Summary:\n")
+   print(best_model)
+   
+   cat("\n=== TRAINING SET ERRORS ===\n")
+   
+   # Calculate training set errors
+   fitted_values <- fitted(best_model)
+   training_errors <- calc_training_errors(fitted_values, y_train)
+   
+   cat("MAPE:", round(training_errors$MAPE, 3), "%\n")
+   cat("RMSE:", round(training_errors$RMSE, 4), "\n")
+   cat("MAE:", round(training_errors$MAE, 4), "\n")
+   cat("R²:", round(training_errors$R2, 4), "\n")
+   
+   # Residual diagnostics
+   cat("\n=== RESIDUAL DIAGNOSTICS ===\n")
+   residuals_test <- Box.test(residuals(best_model), lag = 10, type = "Ljung-Box")
+   cat("Ljung-Box test p-value:", round(residuals_test$p.value, 4), "\n")
+   cat("Residuals independent:", ifelse(residuals_test$p.value > 0.05, "YES", "NO"), "\n")
+   
+ } else {
+   cat("No models converged successfully!\n")
+ }

=== BEST MODEL ANALYSIS ===
Best model: SARIMA( 2 , 0 , 2 )( 1 , 1 , 2 )[168]

Error in optim(init[mask], armafn, method = optim.method, hessian = TRUE,  : 
  non-finite finite-difference value [1]
In addition: Warning message:
In UseMethod("depth") :
  no applicable method for 'depth' applied to an object of class "NULL
