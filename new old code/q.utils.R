# q.utils.R
# -----------------------------------------------------------------------------
# Script with log and helper functions
# -----------------------------------------------------------------------------

log_step <- function(title, expr) {
  cat(sprintf("\n---- %s: START (%s) ----\n", title, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  t0 <- Sys.time()
  on.exit({
    t1 <- Sys.time()
    cat(sprintf("---- %s: END   (%.2f sec) ----\n", title, as.numeric(difftime(t1, t0, units = "secs"))))
  })
  force(expr)
}
