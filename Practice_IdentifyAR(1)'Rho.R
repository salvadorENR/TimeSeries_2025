# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(forecast)

# Function to simulate and plot AR(1) processes
plot_ar1_cases <- function(phi_values) {
  plots <- list()
  
  for (i in seq_along(phi_values)) {
    phi <- phi_values[i]
    
    # Simulate AR(1) process
    set.seed(123)
    n <- 200
    a_t <- rnorm(n)
    z_t <- numeric(n)
    z_t[1] <- a_t[1]
    
    for (t in 2:n) {
      z_t[t] <- phi * z_t[t-1] + a_t[t]
    }
    
    # Create plots
    ts_plot <- ggplot(data.frame(Time = 1:n, Value = z_t), aes(Time, Value)) +
      geom_line(color = "steelblue") +
      ggtitle(paste0("AR(1) Time Series: φ = ", phi)) +
      theme_minimal()
    
    acf_plot <- ggAcf(z_t, lag.max = 20) +
      ggtitle(paste0("ACF: φ = ", phi)) +
      theme_minimal()
    
    pacf_plot <- ggPacf(z_t, lag.max = 20) +
      ggtitle(paste0("PACF: φ = ", phi)) +
      theme_minimal()
    
    # Combine plots
    plots[[i]] <- grid.arrange(ts_plot, acf_plot, pacf_plot, ncol = 3)
  }
  
  return(plots)
}

# Define different phi cases to compare
phi_values <- c(0.2, 0.5, 0.8, -0.5, -0.8, 0.95, -0.95, 1.1)

# Generate and display plots
ar1_plots <- plot_ar1_cases(phi_values)

# Print all plots (will create multiple pages)
for (plot in ar1_plots) {
  print(plot)
}

# Additional analysis for non-stationary cases (|φ| >= 1)
cat("\nNotes on non-stationary cases (|φ| >= 1):\n")
cat("- φ = 1.1: Explosive non-stationary process (ACF decays very slowly)\n")
cat("- φ = 1: Random walk (unit root) - ACF persists near 1 for many lags\n")
cat("- φ = -0.95: Highly oscillatory but stationary\n")


#Practice to learn to guess if Rho is positive or negative
#EX1
set.seed(123)
z <- arima.sim(model = list(ar = 0.7), n = 200)  # True: φ = 0.7 (0 < φ < 1)
par(mfrow = c(1, 2))
acf(z, main = "ACF (Plot 1)")
pacf(z, main = "PACF (Plot 1)")

#EX2
set.seed(123)
z <- arima.sim(model = list(ar = -0.6), n = 200)  # True: φ = -0.6 (-1 < φ < 0)
par(mfrow = c(1, 2))
acf(z, main = "ACF (Plot 2)")
pacf(z, main = "PACF (Plot 2)")

#EX3
set.seed(123)
z <- arima.sim(model = list(ar = 0.3), n = 200)  # True: φ = 0.3 (0 < φ < 1)
par(mfrow = c(1, 2))
acf(z, main = "ACF (Plot 3)")
pacf(z, main = "PACF (Plot 3)")

#EX4
set.seed(123)
z <- arima.sim(model = list(ar = 0.85), n = 200)  # Strong positive autocorrelation
par(mfrow = c(1, 2))
acf(z, main = "ACF (Plot 4)")
pacf(z, main = "PACF (Plot 4)")

#EX5
set.seed(123)
z <- arima.sim(model = list(ar = -0.4), n = 200)  # Moderate negative autocorrelation
par(mfrow = c(1, 2))
acf(z, main = "ACF (Plot 5)")
pacf(z, main = "PACF (Plot 5)")

#EX6
set.seed(123)
z <- arima.sim(model = list(ar = -0.1), n = 200)  # Weak negative autocorrelation
par(mfrow = c(1, 2))
acf(z, main = "ACF (Plot 6)")
pacf(z, main = "PACF (Plot 6)")

#extra
# Random φ between -0.9 and 0.9
phi <- sample(c(-1, 1), 1) * runif(1, 0.1, 0.9)
z <- arima.sim(model = list(ar = phi), n = 200)

# Plot
par(mfrow = c(1, 2))
acf(z, main = paste("ACF: Guess φ =", round(phi, 2)))
pacf(z, main = paste("PACF: Guess φ =", round(phi, 2)))

# Check answer
cat("True φ =", phi)

#Extra 2 (weak and strong oscillations)
set.seed(123)
z <- arima.sim(model = list(ar = -0.1), n = 100)
plot(z, type = "o", col = "gray50", main = "φ = -0.1 (Very Weak Oscillation)")
acf(z, main = "ACF: φ = -0.1")

z <- arima.sim(model = list(ar = -0.3), n = 100)
plot(z, type = "o", col = "orange", main = "φ = -0.3 (Weak Oscillation)")
acf(z, main = "ACF: φ = -0.3")

z <- arima.sim(model = list(ar = -0.5), n = 100)
plot(z, type = "o", col = "blue", main = "φ = -0.5 (Moderate Oscillation)")
acf(z, main = "ACF: φ = -0.5")

z <- arima.sim(model = list(ar = -0.7), n = 100)
plot(z, type = "o", col = "red", main = "φ = -0.7 (Strong Oscillation)")
acf(z, main = "ACF: φ = -0.7")

z <- arima.sim(model = list(ar = -0.9), n = 100)
plot(z, type = "o", col = "purple", main = "φ = -0.9 (Very Strong Oscillation)")
acf(z, main = "ACF: φ = -0.9")