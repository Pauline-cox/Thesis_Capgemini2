> res <- lapply(cands, function(sp) {
+   fit <- fit_arima_robust(y_fit, sp$order, sp$seas, 168, include_mean = FALSE)
+   data.frame(label = sp$label, AIC=AIC(fit), AICc=AICc(fit), BIC=BIC(fit),
+              sigma2 = fit$sigma2, loglik = as.numeric(logLik(fit)),
+              stringsAsFactors=FALSE, fit=I(list(fit)))
+ })
Trying ML, transform.pars=TRUE, optim=default
Trying CSS-ML, transform.pars=TRUE, optim=default
Error in AICc(fit) : could not find function "AICc"
3.
data.frame(label = sp$label, AIC = AIC(fit), AICc = AICc(fit),
BIC = BIC(fit), sigma2 = fit$sigma2, loglik = as.numeric(logLik(fit)),
stringsAsFactors = FALSE, fit = I(list(fit)))
2.
FUN(X[[i]], ...)
1.
lapply(cands, function(sp) {
fit <- fit_arima_robust(y_fit, sp$order, sp$seas, 168, include_mean = FALSE)
data.frame(label = sp$label, AIC = AIC(fit), AICc = AICc(fit),
BIC = BIC(fit), sigma2 = fit$sigma2, loglik = as.numeric(logLik(fit)), ...
