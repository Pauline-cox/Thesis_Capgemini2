suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

# Data
avg <- data.table(
  Inputs = c("Both","Temporal","External"),
  RMSE   = c(45.24114, 46.89216, 50.59106),
  MAE    = c(29.81563, 30.03937, 31.62710),
  MAPE   = c(15.76667, 15.38548, 15.46656)
)



# Ensure we use data.table's melt and keep DT type
avg_long <- data.table::melt(avg, id.vars = "Inputs",
                             variable.name = "Metric", value.name = "Value")
data.table::setDT(avg_long)  # make 100% sure it's a data.table

# Factors
avg_long[, Inputs := factor(Inputs, levels = c("Both","Temporal","External"))]
avg_long[, Metric := factor(Metric, levels = c("RMSE","MAE","MAPE"))]

# Plot
pal <- c("Both"="#3b82f6","Temporal"="#10b981","External"="coral")
p2 <- ggplot(avg_long, aes(x = Metric, y = Value, fill = Inputs)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62, alpha = 0.95) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
  labs(title = "SARIMAX — Comparison input variables",
       x = NULL, y = "Error", fill = "Inputs") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_text(margin = ggplot2::margin(r = 6)))
print(p2)


suppressPackageStartupMessages({ library(data.table); library(ggplot2) })

# --- LSTM averages from your table ---
avg_lstm <- data.table(
  Inputs = c("Both","Temporal","External"),
  RMSE   = c(44.30, 46.55, 81.50),
  MAE    = c(29.14, 31.31, 49.93),
  MAPE   = c(14.93, 16.10, 26.78)
)

# Long format (ensure data.table melt is used)
avg_lstm_long <- data.table::melt(avg_lstm, id.vars = "Inputs",
                                  variable.name = "Metric", value.name = "Value")
data.table::setDT(avg_lstm_long)
avg_lstm_long[, Inputs := factor(Inputs, levels = c("Both","Temporal","External"))]
avg_lstm_long[, Metric := factor(Metric, levels = c("RMSE","MAE","MAPE"))]

# Vibrant palette (same as SARIMAX plot for consistency)
pal <- c("Both" = "#3b82f6",      # blue
         "Temporal" = "#10b981",  # green
         "External" = "coral")  # amber

# Grouped barplot
p_lstm <- ggplot(avg_lstm_long, aes(x = Metric, y = Value, fill = Inputs)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62, alpha = 0.95) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
  labs(title = "LSTM — Comparison input variables",
       x = NULL, y = "Error", fill = "Inputs") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title.y = element_text(margin = ggplot2::margin(r = 6))
  )

print(p_lstm)
# ggsave("lstm_inputs_averages_barplot_grouped.png", p_lstm, width = 7.6, height = 4, dpi = 220)
