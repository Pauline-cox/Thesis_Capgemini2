fit_arima_robust <- function(y, order, seasonal, period,
                             include_mean = FALSE, verbose = TRUE) {
  stopifnot(!any(is.na(y)))
  tries <- list(
    list(method="ML",       transform.pars=TRUE,  optim.method=NULL),
    list(method="CSS-ML",   transform.pars=TRUE,  optim.method=NULL),
    list(method="CSS",      transform.pars=TRUE,  optim.method=NULL),
    list(method="ML",       transform.pars=FALSE, optim.method=NULL),
    list(method="ML",       transform.pars=FALSE, optim.method="BFGS"),
    list(method="ML",       transform.pars=FALSE, optim.method="Nelder-Mead")
  )
  last_err <- NULL
  for (t in tries) {
    if (verbose) message(sprintf("Trying %s, transform.pars=%s, optim=%s",
                                 t$method, t$transform.pars, t$optim.method %||% "default"))
    fit <- tryCatch(
      forecast::Arima(
        y,
        order    = order,
        seasonal = list(order = seasonal, period = period),
        include.mean = include_mean,
        method   = t$method,
        transform.pars = t$transform.pars,
        optim.method   = t$optim.method,
        optim.control  = list(maxit = 2000)
      ),
      error = function(e) e
    )
    if (!inherits(fit, "error")) return(fit)
    last_err <- fit
  }
  stop(last_err)
}
`%||%` <- function(a,b) if (is.null(a)) b else a







y_train <- ts(y_train, frequency = 168)  # ensure m=168
# If you use log1p, transform here:
# y_fit <- log1p(y_train); inv <- function(x) pmax(0, exp(x)-1)
# else:
y_fit <- y_train

cands <- list(
  list(order=c(2,0,2), seas=c(1,1,1), label="(2,0,2)(1,1,1)[168]"),
  list(order=c(2,0,1), seas=c(1,1,2), label="(2,0,1)(1,1,2)[168]"),
  list(order=c(1,0,1), seas=c(1,1,1), label="(1,0,1)(1,1,1)[168]"),
  list(order=c(3,0,1), seas=c(1,1,1), label="(3,0,1)(1,1,1)[168]"),
  list(order=c(2,0,0), seas=c(1,1,1), label="(2,0,0)(1,1,1)[168]")
)

res <- lapply(cands, function(sp) {
  fit <- fit_arima_robust(y_fit, sp$order, sp$seas, 168, include_mean = FALSE)
  data.frame(label = sp$label, AIC=AIC(fit), AICc=AICc(fit), BIC=BIC(fit),
             sigma2 = fit$sigma2, loglik = as.numeric(logLik(fit)),
             stringsAsFactors=FALSE, fit=I(list(fit)))
})

tab <- do.call(rbind, res)[order(vapply(res, function(x) x$AIC, numeric(1))), ]
print(tab[, c("label","AIC","AICc","BIC","sigma2","loglik")])
best <- tab$fit[[1]]




fitted_vals <- fitted(best)
# If you used log1p:
# fitted_vals <- pmax(0, exp(fitted_vals) - 1)

actual <- as.numeric(y_train)
err <- fitted_vals - actual
rmse <- sqrt(mean(err^2)); mae <- mean(abs(err))
mape <- mean(abs(err)/pmax(1e-6, abs(actual))) * 100
r2 <- 1 - sum(err^2)/sum( (actual - mean(actual))^2 )

cat(sprintf("\nTRAIN metrics of %s:\nRMSE=%.1f  MAE=%.1f  MAPE=%.1f%%  R²=%.3f\n",
            tab$label[1], rmse, mae, mape, r2))
