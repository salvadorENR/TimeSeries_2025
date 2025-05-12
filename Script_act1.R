# Load necessary libraries and datasets
library(forecast)    # For time series functions
library(ggplot2)     # For plotting
library(datasets)    # For Nile, LakeHuron
library(fpp2)        # For elecsales, ausbeer, nottem
library(ukgasapi)       # For UKgas dataset

# Function to analyze a time series dataset
analyze_series <- function(data, name) {
  # Step 1: Plot the original series
  p1 <- autoplot(data) + 
    ggtitle(paste("Original Series:", name)) +
    ylab("Value") +
    xlab("Time")
  
  # Step 2: Check for trend/seasonality and apply transformations if needed
  ndiffs_result <- ndiffs(data)
  if (ndiffs_result > 0) {
    diff_data <- diff(data, differences = ndiffs_result)
    transformation <- paste("Applied", ndiffs_result, "order differencing")
  } else {
    diff_data <- data
    transformation <- "No differencing needed"
  }
  
  # Step 3: Plot ACF and PACF of (possibly differenced) series
  p2 <- ggAcf(diff_data) + 
    ggtitle(paste("ACF of", name, "-", transformation))
  
  p3 <- ggPacf(diff_data) + 
    ggtitle(paste("PACF of", name, "-", transformation))
  
  # Display all plots
  gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
  
  # Step 4: Suggest potential models
  cat("\n=== Analysis for", name, "===\n")
  cat("Transformation:", transformation, "\n")
  suggest_model(diff_data)
}

# Helper function to suggest models
suggest_model <- function(data) {
  acf_vals <- Acf(data, plot = FALSE)$acf[-1]
  pacf_vals <- Pacf(data, plot = FALSE)$acf
  
  sig_acf <- sum(abs(acf_vals) > 2/sqrt(length(data)))
  sig_pacf <- sum(abs(pacf_vals) > 2/sqrt(length(data)))
  
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

# Create dataset list with correct names
data_list <- list(
  elecsales = elecsales,  # From fpp2 package
  ausbeer = ausbeer,      # From fpp2 package
  Nile = Nile,           # From datasets package
  nottem = nottem,       # From fpp2 package
  LakeHuron = LakeHuron, # From datasets package
  UKgas = UKgas         # From UKgas package
)

# Analyze each dataset
for (i in seq_along(data_list)) {
  analyze_series(data_list[[i]], names(data_list)[i])
  if (i < length(data_list)) {
    readline(prompt = "Press [enter] to continue to next dataset...")
  }
}