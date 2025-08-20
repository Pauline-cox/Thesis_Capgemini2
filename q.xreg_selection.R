library(data.table)
library(ggplot2)

# ---- 0) Setup ----
stopifnot(is.data.table(model_data))
stopifnot(is.data.table(train_data))
stopifnot("total_consumption_kWh" %in% names(train_data))

# If hour_sin/cos are missing but 'hour' exists, create them
if (!("hour_sin" %in% names(model_data)) && "hour" %in% names(model_data)) {
  model_data[, hour_sin := sin(2*pi*hour/24)]
  model_data[, hour_cos := cos(2*pi*hour/24)]
}

# Candidate pool (keep only those that exist)
candidates <- intersect(c(
  "total_consumption_kWh",
  # history
  "lag_24","lag_48","lag_72","lag_168","lag_336","lag_504",
  "rollmean_24","rollmean_168",
  # building/internal
  "total_occupancy","co2","sound","lux",
  # weather
  "tempC","humidity","temperature","humidity_percent","wind_speed",
  "sunshine_minutes","global_radiation","fog","rain","snow","thunder","ice",
  # calendar/engineered
  "hour_sin","hour_cos","is_weekend","office_hours","holiday"
), names(train_data))

# ---- 1) Build numeric frame on TRAIN ONLY (no leakage) ----
num_dt <- copy(train_data)[, ..candidates]

# keep only numeric columns
num_cols <- names(which(sapply(num_dt, is.numeric)))
num_dt   <- num_dt[, ..num_cols]

# drop near-zero variance cols
nzv <- vapply(num_dt, function(x) (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) > 0, logical(1))
keep_cols <- names(nzv[nzv])
num_dt <- num_dt[, ..keep_cols]

# ---- 2) Spearman correlation on train ----
cor_matrix <- cor(num_dt, use = "pairwise.complete.obs", method = "spearman")

# optional heatmap
cor_dt <- as.data.table(as.table(cor_matrix))
setnames(cor_dt, c("Var1","Var2","Correlation"))
p_corr <- ggplot(cor_dt, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Heatmap (Spearman) — train_data", x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
print(p_corr)

# ---- 3) Rank by |corr with target| ----
target <- "total_consumption_kWh"
others <- setdiff(colnames(cor_matrix), target)
ranked <- sort(abs(cor_matrix[target, others]), decreasing = TRUE)
cat("Top correlations with target (train):\n"); print(head(ranked, 20))

# ---- 4) Greedy de-collinearized pick ----
keep_min      <- 0.10   # raise to 0.15–0.20 to be stricter
collinear_max <- 0.80   # max allowed pairwise |corr| among picked
max_vars      <- 12     # cap total numeric xregs to keep models stable

picked <- character(0)
for (v in names(ranked)) {
  if (abs(cor_matrix[target, v]) < keep_min) next
  if (!length(picked)) {
    picked <- c(picked, v)
  } else {
    max_r <- max(abs(cor_matrix[v, picked]), na.rm = TRUE)
    if (is.finite(max_r) && max_r <= collinear_max) picked <- c(picked, v)
  }
  if (length(picked) >= max_vars) break
}

cat("\nProposed numeric xreg (train-based, low collinearity):\n")
print(picked)

# ---- 5) Final xreg list for SARIMAX (add weekday factor) ----
xreg_cols <- unique(c(picked, intersect("weekday", names(model_data))))
cat("\nUse these in SARIMAX (your helper will one-hot 'weekday'):\n")
print(xreg_cols)


xreg_cols <- c(
  # history (minimal, powerful)
  "lag_24", "lag_168",
  # diurnal cycle
  "hour_sin", "hour_cos",
  # calendar (choose ONE: either weekday OR is_weekend/office_hours)
  "weekday",
  # building
  "total_occupancy", "co2",
  # external
  "global_radiation"   # optional but helpful
)
