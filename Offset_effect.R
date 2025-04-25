# Load required libraries
library(ggplot2)
library(gridExtra)

# Set parameters for the time series
n <- 120  # Number of time points (e.g., 10 years of monthly data)
time <- 1:n
frequency <- 12  # Annual seasonality (12 months)
amplitude <- 5   # Height of the seasonal pattern
mean_level <- 15 # Baseline level

# Create a function to generate time series with different offsets
create_series <- function(offset) {
  seasonal <- amplitude * sin(2 * pi * time / frequency + offset)
  ts_data <- mean_level + seasonal
  data.frame(time = time, value = ts_data, offset = as.factor(round(offset, 2)))
}

# Generate time series with different offsets (in radians)
offsets <- seq(0, 2*pi, length.out = 6)  # 6 different offsets from 0 to 2π
series_list <- lapply(offsets, create_series)
all_series <- do.call(rbind, series_list)

# Plot 1: Individual plots for each offset
individual_plots <- lapply(offsets, function(o) {
  df <- create_series(o)
  ggplot(df, aes(x = time, y = value)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_hline(yintercept = mean_level, linetype = "dashed", color = "red") +
    labs(title = paste("Offset =", round(o, 2), "radians"),
         subtitle = paste("Time shift =", round(-o*frequency/(2*pi), 2), "months"),
         x = "Time", y = "Value") +
    theme_minimal() +
    ylim(mean_level - amplitude - 1, mean_level + amplitude + 1)
})

# Arrange individual plots in a grid
grid.arrange(grobs = individual_plots, ncol = 2, 
             top = "Effect of Different Offsets (Phase Shifts) on Time Series")

# Plot 2: All series together for comparison
ggplot(all_series, aes(x = time, y = value, color = offset)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = mean_level, linetype = "dashed", color = "red") +
  labs(title = "Time Series with Different Offsets (Phase Shifts)",
       subtitle = "Comparing how offsets shift the seasonal pattern in time",
       x = "Time", y = "Value", color = "Offset (radians)") +
  theme_minimal() +
  scale_color_viridis_d() +
  ylim(mean_level - amplitude - 1, mean_level + amplitude + 1) +
  theme(legend.position = "bottom")

# Plot 3: Focus on one cycle to better see the shifts
cycle_data <- all_series[all_series$time <= frequency, ]

ggplot(cycle_data, aes(x = time, y = value, color = offset)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "One Seasonal Cycle with Different Offsets",
       subtitle = "Clear visualization of how offsets shift the pattern",
       x = "Time Point in Cycle", y = "Value", color = "Offset (radians)") +
  theme_minimal() +
  scale_color_viridis_d() +
  ylim(mean_level - amplitude - 1, mean_level + amplitude + 1) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, frequency, by = 1))