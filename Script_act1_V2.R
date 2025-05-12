# Load necessary libraries and datasets
library(forecast)    # For time series tools
library(ggplot2)     # For plotting
library(datasets)    # Nile, LakeHuron
library(fpp2)        # elecsales, ausbeer, nottem
library(ukgasapi)    # UKgas
library(gridExtra)   # Optional, if you still want to arrange plots

# Helper function to suggest models
suggest_model <- function(data) {
  acf_vals <- Acf(data, plot = FALSE)$acf[-1]
  pacf_vals <- Pacf(data, plot = FALSE)$acf
  
  sig_acf <- sum(abs(acf_vals) > 2 / sqrt(length(data)))
  sig_pacf <- sum(abs(pacf_vals) > 2 / sqrt(length(data)))
  
  cat("Significant ACF lags:", sig_acf, "\n")
  cat("Significant PACF lags:", sig_pacf, "\n")
  
  if (sig_acf == 0 && sig_pacf == 0) {
    cat("Suggested model: White noise\n")
  } else if (sig_acf > 0 && sig_pacf == 0) {
    cat("Suggested model: MA(q) where q =", sig_acf, "\n")
  } else if (sig_acf == 0 && sig_pacf > 0) {
    cat("Suggested model: AR(p) where p =", sig_pacf, "\n")
  } else {
    cat("Suggested model: ARMA(p,q) or ARIMA(p,d,q)\n")
    cat("Possible values: p =", sig_pacf, "q =", sig_acf, "\n")
  }
}

# Main function: Analyze each time series
analyze_series <- function(data, name) {
  # Step 1: Original plot
  p1 <- autoplot(data) + 
    ggtitle(paste("Original Series:", name)) +
    ylab("Value") +
    xlab("Time")
  
  # Step 2: Transformation
  ndiffs_result <- ndiffs(data)
  if (ndiffs_result > 0) {
    diff_data <- diff(data, differences = ndiffs_result)
    transformation <- paste("Applied", ndiffs_result, "order differencing")
  } else {
    diff_data <- data
    transformation <- "No differencing needed"
  }
  
  # Step 3: ACF and PACF plots
  p2 <- ggAcf(diff_data) + 
    ggtitle(paste("ACF of", name, "-", transformation))
  
  p3 <- ggPacf(diff_data) + 
    ggtitle(paste("PACF of", name, "-", transformation))
  
  # Step 4: Console report
  cat("\n=== Analysis for", name, "===\n")
  cat("Transformation:", transformation, "\n")
  suggest_model(diff_data)
  
  # Return the 3 plots separately
  return(list(
    original_plot = p1,
    acf_plot = p2,
    pacf_plot = p3
  ))
}

# Dataset list
data_list <- list(
  elecsales = elecsales,     # From fpp2
  ausbeer = ausbeer,         # From fpp2
  Nile = Nile,               # From datasets
  nottem = nottem,           # From fpp2
  LakeHuron = LakeHuron,     # From datasets
  UKgas = UKgas              # From ukgasapi
)

# Analyze all series one by one and show plots separately
for (i in seq_along(data_list)) {
  name <- names(data_list)[i]
  result <- analyze_series(data_list[[i]], name)
  
  # Show each plot separately
  print(result$original_plot)
  readline(prompt = "Press [enter] to view ACF...")
  print(result$acf_plot)
  readline(prompt = "Press [enter] to view PACF...")
  print(result$pacf_plot)
  
  if (i < length(data_list)) {
    readline(prompt = "Press [enter] to continue to the next dataset...")
  }
}
