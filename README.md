suppressPackageStartupMessages({ library(forecast) })

# --- 1) Target series (no xreg) ---
y_raw <- if (exists("model_data") && !is.null(model_data$y)) {
  as.numeric(model_data$y)
} else {
  as.numeric(clean_data$total_consumption_kWh)
}
y_raw <- y_raw[is.finite(y_raw)]
stopifnot(length(y_raw) > 0)

m <- 168
y_ts <- ts(y_raw, frequency = m)

# --- 2) Refit specified orders: ARIMA(3,0,1)(0,1,0)[168] ---
fit_try <- function(method) try(Arima(
  y_ts,
  order = c(3,0,1),
  seasonal = list(order = c(0,1,0), period = m),
  method = method,
  include.mean = FALSE,   # D=1 → no mean/intercept
  optim.control = list(maxit = 1000)
), silent = TRUE)

fit <- fit_try("CSS-ML")
if (inherits(fit, "try-error")) fit <- fit_try("ML")
if (inherits(fit, "try-error")) fit <- fit_try("CSS")
if (inherits(fit, "try-error")) stop("Refit failed for all methods.")

cat("Fitted with method:", fit$method, "\n")
print(summary(fit))

# --- 3) Diagnostics ---
e  <- residuals(fit)
lb <- Box.test(e, lag = 2*m, type = "Ljung-Box", fitdf = sum(!is.na(coef(fit))))
cat(sprintf("Ljung-Box p (lag=%d): %.4f\n", 2*m, lb$p.value))
cat(sprintf("Train RMSE: %.3f | MAE: %.3f\n", sqrt(mean(e^2)), mean(abs(e))))

# --- 4) 24-hour forecast (no xreg) ---
h  <- 24
fc <- forecast(fit, h = h)
print(fc)
pred_24h <- as.numeric(fc$mean)
