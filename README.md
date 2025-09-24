# --- helpers (add once near the top) ---
AICc_safe <- function(model){
  n <- length(residuals(model)); k <- length(coef(model)) + 1
  stats::AIC(model) + (2*k*(k+1)) / (n - k - 1)
}
`%||%` <- function(a,b) if (is.null(a)) b else a

# --- your existing robust fitter should already be defined: fit_arima_robust(...) ---

# --- patched comparison block ---
res_list <- lapply(cands, function(sp) {
  fit <- tryCatch(
    fit_arima_robust(y_fit, sp$order, sp$seas, 168, include_mean = FALSE, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(data.frame(
      label = sp$label, AIC=NA, AICc=NA, BIC=NA, sigma2=NA, loglik=NA,
      converged = FALSE, stringsAsFactors = FALSE
    ))
  }
  data.frame(
    label   = sp$label,
    AIC     = AIC(fit),
    AICc    = AICc_safe(fit),   # <- use helper
    BIC     = BIC(fit),
    sigma2  = fit$sigma2,
    loglik  = as.numeric(logLik(fit)),
    converged = TRUE,
    fit     = I(list(fit)),
    stringsAsFactors = FALSE
  )
})

tab <- do.call(rbind, res_list)
tab_ok <- subset(tab, converged & is.finite(AICc))
tab_ok <- tab_ok[order(tab_ok$AICc), ]

cat("\n=== Candidate ranking (by AICc) ===\n")
print(tab_ok[, c("label","AIC","AICc","BIC","sigma2","loglik")], row.names = FALSE)

if (nrow(tab_ok)) {
  best <- tab_ok$fit[[1]]
  cat("\n=== Chosen model ===\n")
  print(best)
} else {
  cat("\nNo models converged.\n")
}
