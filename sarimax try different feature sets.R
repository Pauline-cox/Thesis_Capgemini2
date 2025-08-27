
# ---- 0) Encode categorical vars --------------------------------------------
train_dt[, is_weekend := as.integer(is_weekend)]
test_dt[,  is_weekend := as.integer(is_weekend)]

train_dt[, office_hours := as.integer(office_hours)]
test_dt[,  office_hours := as.integer(office_hours)]

train_dt[, holiday := as.integer(holiday)]
test_dt[,  holiday := as.integer(holiday)]

# (weekday is factor -> skip for now to avoid collinearity with seasonal cycle)

# ---- 1) Helper function ----------------------------------------------------
evaluate_sarimax <- function(y_train, y_test, train_dt, test_dt, vars,
                             order=c(1,0,1), seas=list(order=c(1,1,1), period=168)) {
  Xtr <- as.matrix(train_dt[, ..vars])
  Xte <- as.matrix(test_dt[, ..vars])
  
  fit <- tryCatch(
    Arima(
      y_train,
      order = order,
      seasonal = seas,
      xreg = Xtr,
      include.mean = TRUE,
      method = "CSS"
    ),
    error = function(e) return(NULL)
  )
  if (is.null(fit)) return(NULL)
  
  fc <- forecast(fit, h = length(y_test), xreg = Xte)
  res_dt <- data.table(actual=y_test, forecast=as.numeric(fc$mean))
  
  MSE  <- mean((res_dt$actual - res_dt$forecast)^2, na.rm=TRUE)
  RMSE <- sqrt(MSE)
  MAE  <- mean(abs(res_dt$actual - res_dt$forecast), na.rm=TRUE)
  MAPE <- mean(abs((res_dt$actual - res_dt$forecast)/res_dt$actual), na.rm=TRUE)*100
  
  list(
    vars = paste(vars, collapse=", "),
    MSE = MSE, RMSE = RMSE, MAE = MAE, MAPE = MAPE
  )
}

# ---- 2) Candidate sets -----------------------------------------------------
candidate_sets <- list(
  c(),  # baseline SARIMA
  c("total_occupancy"),
  c("total_occupancy","co2"),
  c("tempC","humidity","global_radiation"),
  c("total_occupancy","tempC"),
  c("total_occupancy","tempC","co2"),
  c("total_occupancy","tempC","lag_24"),
  c("total_occupancy","tempC","co2","lag_24"),
  c("total_occupancy","tempC","co2","lux","lag_24","is_weekend","office_hours"),
  c("total_occupancy","tempC","co2","lag_24","rollmean_24","rollsd_24")
)

candidate_sets <- list(
  # 0. Baseline (no exogenous)
  c(),  
  
  # 1. Occupancy only (pure behavioral driver)
  c("total_occupancy"),
  
  # 2. Occupancy + CO2 (occupancy + indoor proxy)
  c("total_occupancy","co2"),
  
  # 3. Weather only (HVAC driver block)
  c("tempC","humidity","global_radiation"),
  
  # 4. Occupancy + weather (human + climate)
  c("total_occupancy","tempC"),
  c("total_occupancy","tempC","co2"),
  
  # 5. Persistence effects (add lag)
  c("total_occupancy","co2","lag_24"),        # best performer in your tests
  c("total_occupancy","tempC","co2","lag_24"),
  
  # 6. Enriched with environment/lighting
  c("total_occupancy","co2","lux","lag_24"),
  c("total_occupancy","co2","lux","lag_24","global_radiation"),
  
  # 7. Time/context indicators
  c("total_occupancy","co2","lag_24","is_weekend","office_hours"),
  
  # 8. With rolling stats (exploratory, risk of overfit)
  c("total_occupancy","co2","lag_24","rollmean_24","rollsd_24")
)


# ---- 3) Run all experiments ------------------------------------------------
results <- lapply(candidate_sets, function(vars) {
  evaluate_sarimax(y_train, y_test, train_dt, test_dt, vars)
})

results_dt <- rbindlist(results, fill=TRUE)
setorder(results_dt, MAPE)

# ---- 4) Print nicely -------------------------------------------------------
print(results_dt)
results_dt[, sprintf("Vars: [%s] | RMSE=%.2f | MAPE=%.2f%%", vars, RMSE, MAPE)]
