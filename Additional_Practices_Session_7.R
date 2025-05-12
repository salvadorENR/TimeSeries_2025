#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Illustration of invertibility 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load required libraries
library(ggplot2)
library(gridExtra)
library(forecast)

# Set seed for reproducibility
set.seed(123)

## Function to simulate and plot MA(1) processes
plot_ma1_comparison <- function(theta) {
  # Simulate MA(1) process
  n <- 200
  ma1_sim <- arima.sim(n = n, model = list(ma = theta))
  
  # Create time series plot
  ts_plot <- autoplot(ma1_sim) + 
    ggtitle(paste0("MA(1) Process: θ = ", theta)) +
    ylab("Value") + 
    xlab("Time") +
    theme_minimal()
  
  # Create ACF plot
  acf_plot <- ggAcf(ma1_sim) + 
    ggtitle("Autocorrelation Function (ACF)") +
    theme_minimal()
  
  # Create PACF plot
  pacf_plot <- ggPacf(ma1_sim) + 
    ggtitle("Partial Autocorrelation Function (PACF)") +
    theme_minimal()
  
  # Create impulse response plot
  impulse_response <- data.frame(lag = 0:10, weight = c(1, -theta, rep(0, 9)))  # MA(1) has only one lag
    
    ir_plot <- ggplot(impulse_response, aes(x = lag, y = weight)) +
      geom_col(fill = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ggtitle("Impulse Response Weights") +
      xlab("Lag") + ylab("Weight") +
      scale_x_continuous(breaks = 0:10) +
      theme_minimal()
    
    # Combine all plots
    grid.arrange(ts_plot, acf_plot, pacf_plot, ir_plot, 
                 ncol = 2, 
                 top = paste0("MA(1) Process Analysis (θ = ", theta, ")"))
}

## Compare invertible vs non-invertible cases
# Case 1: Invertible MA(1) with θ = 0.6 (|θ| < 1)
plot_ma1_comparison(0.6)

# Case 2: Non-invertible MA(1) with θ = 1.2 (|θ| > 1)
plot_ma1_comparison(1.2)

# Case 3: Boundary case with θ = 1 (|θ| = 1)
plot_ma1_comparison(1.0)

## Additional visualization: AR(∞) representation convergence
visualize_ar_representation <- function(theta) {
  max_lags <- 20
  weights <- (-theta)^(0:max_lags)
  
  convergence_plot <- ggplot(data.frame(lag = 0:max_lags, weight = weights), 
                             aes(x = lag, y = weight)) +
    geom_col(fill = ifelse(abs(theta) < 1, "steelblue", "salmon")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ggtitle(paste0("AR(∞) Representation Weights\nθ = ", theta, 
                   " (", ifelse(abs(theta) < 1, "Invertible", "Non-invertible"), ")")) +
    xlab("Lag") + ylab("Weight") +
    theme_minimal()
  
  print(convergence_plot)
  
  if (abs(theta) < 1) {
    cat("Weights converge to 0 because |θ| < 1\n")
    cat("Sum of absolute weights:", sum(abs(weights)), "\n")
  } else {
    cat("Weights diverge because |θ| ≥ 1\n")
    cat("Sum of absolute weights: Infinite\n")
  }
}

# Visualize AR(∞) representation for different θ values
visualize_ar_representation(0.6)  # Invertible case
visualize_ar_representation(1.2)  # Non-invertible case


theta <- 0.6  # MA(1) coefficient
lags <- 0:10  # Number of lags to plot
weights <- (-theta)^lags  # AR(∞) weights

plot(lags, weights, type = "h", lwd = 2, col = "steelblue",
     main = paste0("AR(∞) Weights for MA(1) with θ = ", theta),
     xlab = "Lag k", ylab = expression(paste("Weight ", (-θ)^k)))
abline(h = 0, lty = 2)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++