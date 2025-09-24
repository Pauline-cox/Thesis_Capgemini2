> cat("Starting SARIMAX Grid Search\n")
Starting SARIMAX Grid Search
> cat("Total combinations:", nrow(param_grid), "\n")
Total combinations: 36 
> cat("Estimated time:", nrow(param_grid) * 20, "minutes\n")
Estimated time: 720 minutes
> cat("===========================================\n\n")
===========================================

> start_time <- Sys.time()
> for (i in 1:nrow(param_grid)) {
+   cat("Progress:", i, "/", nrow(param_grid), "\n")
+   cat("Testing SARIMA(", param_grid$p[i], ",", param_grid$d[i], ",", param_grid$q[i], 
+       ")(", param_grid$P[i], ",", param_grid$D[i], ",", param_grid$Q[i], ")[", 
+       param_grid$seasonal[i], "]\n")
+   
+   iter_start <- Sys.time()
+   
+   tryCatch({
+     # Fit SARIMAX model
+     model <- Arima(y_train,
+                    order = c(param_grid$p[i], param_grid$d[i], param_grid$q[i]),
+                    seasonal = list(order = c(param_grid$P[i], param_grid$D[i], param_grid$Q[i]), 
+                                    period = param_grid$seasonal[i]),
+                    # xreg = xreg_train,  # Uncomment when you have exogenous variables
+                    method = "CSS-ML")
+     
+     # Store results
+     results[i, ] <- data.frame(
+       p = param_grid$p[i], d = param_grid$d[i], q = param_grid$q[i],
+       P = param_grid$P[i], D = param_grid$D[i], Q = param_grid$Q[i],
+       AIC = model$aic, AICc = model$aicc, BIC = model$bic,
+       loglik = model$loglik, sigma2 = model$sigma2,
+       convergence = TRUE, error_msg = ""
+     )
+     
+     cat("✓ Success - AIC:", round(model$aic, 2), "BIC:", round(model$bic, 2), "\n")
+     
+   }, error = function(e) {
+     # Store error information
+     results[i, ] <- data.frame(
+       p = param_grid$p[i], d = param_grid$d[i], q = param_grid$q[i],
+       P = param_grid$P[i], D = param_grid$D[i], Q = param_grid$Q[i],
+       AIC = NA, AICc = NA, BIC = NA,
+       loglik = NA, sigma2 = NA,
+       convergence = FALSE, error_msg = as.character(e$message)
+     )
+     
+     cat("✗ Error:", e$message, "\n")
+   })
+   
+   iter_end <- Sys.time()
+   iter_time <- as.numeric(difftime(iter_end, iter_start, units = "mins"))
+   
+   cat("Iteration time:", round(iter_time, 2), "minutes\n")
+   
+   # Estimate remaining time
+   elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
+   avg_time_per_iter <- elapsed_time / i
+   remaining_time <- (nrow(param_grid) - i) * avg_time_per_iter
+   
+   cat("Elapsed:", round(elapsed_time, 1), "min | Remaining:", round(remaining_time, 1), "min\n")
+   cat("-------------------------------------------\n\n")
+ }
Progress: 1 / 36 
Testing SARIMA( 2 , 0 , 0 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153368.6 BIC: 153391.5 
Iteration time: 0.61 minutes
Elapsed: 0.6 min | Remaining: 21.5 min
-------------------------------------------

Progress: 2 / 36 
Testing SARIMA( 3 , 0 , 0 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153240.8 BIC: 153271.3 
Iteration time: 1.34 minutes
Elapsed: 2 min | Remaining: 33.2 min
-------------------------------------------

Progress: 3 / 36 
Testing SARIMA( 4 , 0 , 0 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153140.9 BIC: 153179 
Iteration time: 2.02 minutes
Elapsed: 4 min | Remaining: 43.7 min
-------------------------------------------

Progress: 4 / 36 
Testing SARIMA( 2 , 0 , 1 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153333.8 BIC: 153364.3 
Iteration time: 1.18 minutes
Elapsed: 5.2 min | Remaining: 41.2 min
-------------------------------------------

Progress: 5 / 36 
Testing SARIMA( 3 , 0 , 1 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153128.1 BIC: 153166.2 
Iteration time: 2.23 minutes
Elapsed: 7.4 min | Remaining: 45.8 min
-------------------------------------------

Progress: 6 / 36 
Testing SARIMA( 4 , 0 , 1 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153124.9 BIC: 153170.6 
Iteration time: 3.34 minutes
Elapsed: 10.7 min | Remaining: 53.6 min
-------------------------------------------

Progress: 7 / 36 
Testing SARIMA( 2 , 0 , 2 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153126.2 BIC: 153164.3 
Iteration time: 2.25 minutes
Elapsed: 13 min | Remaining: 53.7 min
-------------------------------------------

Progress: 8 / 36 
Testing SARIMA( 3 , 0 , 2 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153126.1 BIC: 153171.9 
Iteration time: 3.71 minutes
Elapsed: 16.7 min | Remaining: 58.4 min
-------------------------------------------

Progress: 9 / 36 
Testing SARIMA( 4 , 0 , 2 )( 0 , 1 , 0 )[ 168 ]
✓ Success - AIC: 153123.9 BIC: 153177.2 
Iteration time: 12.61 minutes
Elapsed: 29.3 min | Remaining: 87.9 min
-------------------------------------------

Progress: 10 / 36 
Testing SARIMA( 2 , 0 , 0 )( 1 , 1 , 0 )[ 168 ]
✗ Error: non-finite finite-difference value [1] 
Iteration time: 0.39 minutes
Elapsed: 29.7 min | Remaining: 77.2 min
-------------------------------------------

Progress: 11 / 36 
Testing SARIMA( 3 , 0 , 0 )( 1 , 1 , 0 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.14 minutes
Elapsed: 29.8 min | Remaining: 67.8 min
-------------------------------------------

Progress: 12 / 36 
Testing SARIMA( 4 , 0 , 0 )( 1 , 1 , 0 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.15 minutes
Elapsed: 30 min | Remaining: 60 min
-------------------------------------------

Progress: 13 / 36 
Testing SARIMA( 2 , 0 , 1 )( 1 , 1 , 0 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.15 minutes
Elapsed: 30.1 min | Remaining: 53.3 min
-------------------------------------------

Progress: 14 / 36 
Testing SARIMA( 3 , 0 , 1 )( 1 , 1 , 0 )[ 168 ]
✗ Error: non-finite finite-difference value [2] 
Iteration time: 0.67 minutes
Elapsed: 30.8 min | Remaining: 48.4 min
-------------------------------------------

Progress: 15 / 36 
Testing SARIMA( 4 , 0 , 1 )( 1 , 1 , 0 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.16 minutes
Elapsed: 31 min | Remaining: 43.3 min
-------------------------------------------

Progress: 16 / 36 
Testing SARIMA( 2 , 0 , 2 )( 1 , 1 , 0 )[ 168 ]
✓ Success - AIC: 149353.6 BIC: 149399.4 
Iteration time: 20.7 minutes
Elapsed: 51.7 min | Remaining: 64.6 min
-------------------------------------------

Progress: 17 / 36 
Testing SARIMA( 3 , 0 , 2 )( 1 , 1 , 0 )[ 168 ]
✗ Error: non-finite finite-difference value [1] 
Iteration time: 3.51 minutes
Elapsed: 55.2 min | Remaining: 61.6 min
-------------------------------------------

Progress: 18 / 36 
Testing SARIMA( 4 , 0 , 2 )( 1 , 1 , 0 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.13 minutes
Elapsed: 55.3 min | Remaining: 55.3 min
-------------------------------------------

Progress: 19 / 36 
Testing SARIMA( 2 , 0 , 0 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146848.1 BIC: 146878.7 
Iteration time: 9.93 minutes
Elapsed: 65.2 min | Remaining: 58.4 min
-------------------------------------------

Progress: 20 / 36 
Testing SARIMA( 3 , 0 , 0 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146742.5 BIC: 146780.6 
Iteration time: 13.85 minutes
Elapsed: 79.1 min | Remaining: 63.3 min
-------------------------------------------

Progress: 21 / 36 
Testing SARIMA( 4 , 0 , 0 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146692.2 BIC: 146738 
Iteration time: 18.77 minutes
Elapsed: 97.8 min | Remaining: 69.9 min
-------------------------------------------

Progress: 22 / 36 
Testing SARIMA( 2 , 0 , 1 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146836.4 BIC: 146874.5 
Iteration time: 13.52 minutes
Elapsed: 111.4 min | Remaining: 70.9 min
-------------------------------------------

Progress: 23 / 36 
Testing SARIMA( 3 , 0 , 1 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146678.7 BIC: 146724.5 
Iteration time: 19.07 minutes
Elapsed: 130.4 min | Remaining: 73.7 min
-------------------------------------------

Progress: 24 / 36 
Testing SARIMA( 4 , 0 , 1 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146680.5 BIC: 146733.9 
Iteration time: 27.52 minutes
Elapsed: 158 min | Remaining: 79 min
-------------------------------------------

Progress: 25 / 36 
Testing SARIMA( 2 , 0 , 2 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146679.8 BIC: 146725.5 
Iteration time: 22.52 minutes
Elapsed: 180.5 min | Remaining: 79.4 min
-------------------------------------------

Progress: 26 / 36 
Testing SARIMA( 3 , 0 , 2 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146680.6 BIC: 146734 
Iteration time: 30.05 minutes
Elapsed: 210.5 min | Remaining: 81 min
-------------------------------------------

Progress: 27 / 36 
Testing SARIMA( 4 , 0 , 2 )( 0 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146681.5 BIC: 146742.5 
Iteration time: 36.82 minutes
Elapsed: 247.4 min | Remaining: 82.5 min
-------------------------------------------

Progress: 28 / 36 
Testing SARIMA( 2 , 0 , 0 )( 1 , 1 , 1 )[ 168 ]
✗ Error: non-finite finite-difference value [4] 
Iteration time: 2.54 minutes
Elapsed: 249.9 min | Remaining: 71.4 min
-------------------------------------------

Progress: 29 / 36 
Testing SARIMA( 3 , 0 , 0 )( 1 , 1 , 1 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.17 minutes
Elapsed: 250.1 min | Remaining: 60.4 min
-------------------------------------------

Progress: 30 / 36 
Testing SARIMA( 4 , 0 , 0 )( 1 , 1 , 1 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.18 minutes
Elapsed: 250.2 min | Remaining: 50 min
-------------------------------------------

Progress: 31 / 36 
Testing SARIMA( 2 , 0 , 1 )( 1 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146829.7 BIC: 146875.5 
Iteration time: 29.07 minutes
Elapsed: 279.3 min | Remaining: 45.1 min
-------------------------------------------

Progress: 32 / 36 
Testing SARIMA( 3 , 0 , 1 )( 1 , 1 , 1 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.19 minutes
Elapsed: 279.5 min | Remaining: 34.9 min
-------------------------------------------

Progress: 33 / 36 
Testing SARIMA( 4 , 0 , 1 )( 1 , 1 , 1 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.21 minutes
Elapsed: 279.7 min | Remaining: 25.4 min
-------------------------------------------

Progress: 34 / 36 
Testing SARIMA( 2 , 0 , 2 )( 1 , 1 , 1 )[ 168 ]
✓ Success - AIC: 146673.8 BIC: 146727.2 
Iteration time: 32.37 minutes
Elapsed: 312.1 min | Remaining: 18.4 min
-------------------------------------------

Progress: 35 / 36 
Testing SARIMA( 3 , 0 , 2 )( 1 , 1 , 1 )[ 168 ]
✗ Error: non-finite finite-difference value [3] 
Iteration time: 1.03 minutes
Elapsed: 313.1 min | Remaining: 8.9 min
-------------------------------------------

Progress: 36 / 36 
Testing SARIMA( 4 , 0 , 2 )( 1 , 1 , 1 )[ 168 ]
✗ Error: initial value in 'vmmin' is not finite 
Iteration time: 0.17 minutes
Elapsed: 313.3 min | Remaining: 0 min
-------------------------------------------

> end_time <- Sys.time()
> total_time <- difftime(end_time, start_time, units = "mins")
> cat("Grid Search Complete!\n")
Grid Search Complete!
> cat("Total time:", round(total_time, 2), "minutes\n\n")
Total time: 313.29 minutes

> # Display results sorted by AIC
> results_clean <- results[results$convergence == TRUE, ]
> results_sorted <- results_clean[order(results_clean$AIC), ]
> cat("=== GRID SEARCH RESULTS (sorted by AIC) ===\n")
=== GRID SEARCH RESULTS (sorted by AIC) ===
> print(results_sorted[, c("p", "d", "q", "P", "D", "Q", "AIC", "AICc", "BIC", "loglik")])
       p  d  q  P  D  Q      AIC     AICc      BIC    loglik
34     2  0  2  1  1  1 146673.8 146673.8 146727.2 -73329.89
23     3  0  1  0  1  1 146678.7 146678.7 146724.5 -73333.35
25     2  0  2  0  1  1 146679.8 146679.8 146725.5 -73333.88
24     4  0  1  0  1  1 146680.5 146680.5 146733.9 -73333.27
26     3  0  2  0  1  1 146680.6 146680.6 146734.0 -73333.32
27     4  0  2  0  1  1 146681.5 146681.5 146742.5 -73332.76
21     4  0  0  0  1  1 146692.2 146692.2 146738.0 -73340.11
20     3  0  0  0  1  1 146742.5 146742.5 146780.6 -73366.23
31     2  0  1  1  1  1 146829.7 146829.7 146875.5 -73408.86
22     2  0  1  0  1  1 146836.4 146836.4 146874.5 -73413.19
19     2  0  0  0  1  1 146848.2 146848.2 146878.7 -73420.08
16     2  0  2  1  1  0 149353.6 149353.6 149399.4 -74670.80
9      4  0  2  0  1  0 153123.8 153123.9 153177.2 -76554.92
6      4  0  1  0  1  0 153124.9 153124.9 153170.6 -76556.44
8      3  0  2  0  1  0 153126.2 153126.2 153171.9 -76557.08
7      2  0  2  0  1  0 153126.2 153126.2 153164.3 -76558.09
5      3  0  1  0  1  0 153128.1 153128.1 153166.2 -76559.03
3      4  0  0  0  1  0 153140.9 153140.9 153179.0 -76565.45
2      3  0  0  0  1  0 153240.8 153240.8 153271.3 -76616.40
4      2  0  1  0  1  0 153333.8 153333.8 153364.3 -76662.88
1      2  0  0  0  1  0 153368.6 153368.6 153391.5 -76681.31
NA    NA NA NA NA NA NA       NA       NA       NA        NA
NA.1  NA NA NA NA NA NA       NA       NA       NA        NA
NA.2  NA NA NA NA NA NA       NA       NA       NA        NA
NA.3  NA NA NA NA NA NA       NA       NA       NA        NA
NA.4  NA NA NA NA NA NA       NA       NA       NA        NA
NA.5  NA NA NA NA NA NA       NA       NA       NA        NA
NA.6  NA NA NA NA NA NA       NA       NA       NA        NA
NA.7  NA NA NA NA NA NA       NA       NA       NA        NA
NA.8  NA NA NA NA NA NA       NA       NA       NA        NA
NA.9  NA NA NA NA NA NA       NA       NA       NA        NA
NA.10 NA NA NA NA NA NA       NA       NA       NA        NA
NA.11 NA NA NA NA NA NA       NA       NA       NA        NA
NA.12 NA NA NA NA NA NA       NA       NA       NA        NA
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
+                       method = "CSS-ML")
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
Best model: SARIMA( 2 , 0 , 2 )( 1 , 1 , 1 )[168]

Model Summary:
Series: y_train 
ARIMA(2,0,2)(1,1,1)[168] 

Coefficients:
         ar1      ar2      ma1     ma2    sar1     sma1
      1.6288  -0.6668  -0.7249  0.0910  0.0323  -0.7751
s.e.  0.0312   0.0284   0.0321  0.0088  0.0114   0.0080

sigma^2 = 917.6:  log likelihood = -73329.89
AIC=146673.8   AICc=146673.8   BIC=146727.2

=== TRAINING SET ERRORS ===
MAPE: 11.536 %
RMSE: 30.1202 
MAE: 17.1762 
R²: 0.9678 

=== RESIDUAL DIAGNOSTICS ===
Ljung-Box test p-value: 0 
Residuals independent: NO 
