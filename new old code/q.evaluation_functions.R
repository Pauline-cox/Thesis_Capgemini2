# q.evaluation_functions.R
# -----------------------------------------------------------------------------
# functions that print and plot forecast and residual evaluation
# -----------------------------------------------------------------------------


# ---------- evaluation (MAE, RMSE, MAPE, R2) ----------
eval_metrics <- function(actual, pred) {
  stopifnot(length(actual) == length(pred))
  ok <- is.finite(actual) & is.finite(pred)
  
  sse <- sum((pred[ok] - actual[ok])^2)
  sst <- sum((actual[ok] - mean(actual[ok]))^2)
  r2  <- if (sst > 0) (1 - sse / sst) else NA_real_
  
  ae   <- abs(pred[ok] - actual[ok])
  rmse <- sqrt(mean((pred[ok] - actual[ok])^2))
  mae  <- mean(ae)
  mape <- mean(ae / pmax(1e-8, abs(actual[ok]))) * 100
  
  data.table(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r2)
}
