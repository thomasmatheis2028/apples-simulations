library(tidyverse)

df <- missing_data
df <- df %>% mutate(method = fct_reorder(method, type1_error))

# ---- Dual-axis strategy ----
# ggplot2 has no built-in dual-axis bar chart. The workaround:
# 1. Pick a "primary" scale (here: type1_error, left axis)
# 2. Rescale the secondary variable (mean_F) into the primary's range
#    using a linear transform: scaled = (value - min)/(max-min) * range_primary
# 3. Plot type1_error as bars (left axis) and mean_F as a line/points
#    using the SAME scaled values
# 4. Add sec_axis() with the INVERSE transform so the right-axis labels
#    show the true mean_F values

r1 <- range(df$type1_error)
r2 <- range(df$mean_F)

# linear map from r2 (F range) -> r1 (error range)
scale_to_primary <- function(x) {
  (x - r2[1]) / (r2[2] - r2[1]) * (r1[2] - r1[1]) + r1[1]
}
# inverse map, used only to draw correct tick labels on the right axis
inverse_to_secondary <- function(x) {
  (x - r1[1]) / (r1[2] - r1[1]) * (r2[2] - r2[1]) + r2[1]
}

df <- df %>% mutate(mean_F_scaled = scale_to_primary(mean_F))

dual_plot <- ggplot(df, aes(x = method)) +
  geom_col(aes(y = type1_error, fill = "Type I Error")) +
  geom_line(aes(y = mean_F_scaled, group = 1, color = "Mean F-Statistic"),
            linewidth = 1) +
  geom_point(aes(y = mean_F_scaled, color = "Mean F-Statistic"), size = 3) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "grey40") +
  scale_y_continuous(
    name = "Type I Error",
    sec.axis = sec_axis(~ inverse_to_secondary(.), name = "Mean F-Statistic")
  ) +
  scale_fill_manual(name = NULL, values = c("Type I Error" = "steelblue")) +
  scale_color_manual(name = NULL, values = c("Mean F-Statistic" = "firebrick")) +
  labs(title = "Type I Error and Mean F-Statistic by Imputation Method",
       x = "Imputation Method") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

dual_plot

ggsave("dual_axis_plot.png", dual_plot, width = 8, height = 6, dpi = 300)