#---------------------------------------------------------------------------
# 1. Plots
#---------------------------------------------------------------------------

# Load necessary libraries and datasets
library(forecast)    # For time series functions
library(ggplot2)     # For plotting
library(datasets)    # For Nile, LakeHuron
library(fpp2)        # For elecsales, ausbeer, nottem
library(ukgasapi)    # For UKgas dataset
library(magrittr)    # For pipe (%>%)
library(gridExtra)   # For arranging plots

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
  elecsales = elecsales,
  ausbeer = ausbeer,
  Nile = Nile,
  nottem = nottem,
  LakeHuron = LakeHuron,
  UKgas = UKgas
)

# Analyze each dataset
for (i in seq_along(data_list)) {
  analyze_series(data_list[[i]], names(data_list)[i])
  if (i < length(data_list)) {
    readline(prompt = "Press [enter] to continue to next dataset...")
  }
}

#---------------------------------------------------------------------
# 2. Differentiation 
#---------------------------------------------------------------------

# ELECSALES
elecsales_diff <- diff(elecsales, differences = 1)
autoplot(elecsales_diff) +
  ggtitle("elecsales - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# AUSBEER
ausbeer_diff <- diff(diff(ausbeer, lag = 4), differences = 1)
autoplot(ausbeer_diff) +
  ggtitle("ausbeer - Serie diferenciada (regular + estacional)") +
  xlab("Tiempo") + ylab("Diferencia")

# NILE
Nile_diff <- diff(Nile, differences = 1)
autoplot(Nile_diff) +
  ggtitle("Nile - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# NOTTEM (solo estacional)
nottem_diff <- diff(diff(nottem, lag = 12))
autoplot(nottem_diff) +
  ggtitle("nottem - Serie diferenciada (estacional)") +
  ylab("Diferencia") + xlab("Tiempo")

# LAKEHURON
LakeHuron_diff <- diff(LakeHuron, differences = 1)
autoplot(LakeHuron_diff) +
  ggtitle("LakeHuron - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# UKGAS
UKgas_diff <- diff(diff(UKgas, lag = 4), differences = 1)
autoplot(UKgas_diff) +
  ggtitle("UKgas - Serie diferenciada (regular + estacional)") +
  xlab("Tiempo") + ylab("Diferencia")

#-------------------------------------------------------------------------------
# 3. ACF and PACF (with thicker lines)
#-------------------------------------------------------------------------------

# ELECSALES
elecsales_diff <-diff(diff(elecsales, lag = 4), differences = 1)

  
ggAcf(elecsales_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("ACF - elecsales (diferenciada)")

ggPacf(elecsales_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("PACF - elecsales (diferenciada)")

ggAcf(elecsales_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("ACF - elecsales (diferenciada)")

ggPacf(elecsales_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("PACF - elecsales (diferenciada)")

# AUSBEER
ggAcf(ausbeer_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("ACF - ausbeer (diferenciada)")

ggPacf(ausbeer_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("PACF - ausbeer (diferenciada)")

# NILE
ggAcf(Nile_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("ACF - Nile (diferenciada)")

ggPacf(Nile_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("PACF - Nile (diferenciada)")

# NOTTEM
ggAcf(nottem_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("ACF - nottem (diferenciada)")

ggPacf(nottem_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("PACF - nottem (diferenciada)")

# LAKEHURON
ggAcf(LakeHuron_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("ACF - LakeHuron (diferenciada)")

ggPacf(LakeHuron_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("PACF - LakeHuron (diferenciada)")

# UKGAS
ggAcf(UKgas_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("ACF - UKgas (diferenciada)")

ggPacf(UKgas_diff, plot = FALSE) %>% autoplot() +
  geom_segment(size = 1.2) +
  ggtitle("PACF - UKgas (diferenciada)")

#------------------------------------------------------------------------------
# 4. Automatic ARIMA Model Suggestion
#------------------------------------------------------------------------------

auto.arima(elecsales)
auto.arima(ausbeer)
auto.arima(ausbeer_diff)
auto.arima(Nile)
auto.arima(nottem)
auto.arima(LakeHuron)
auto.arima(UKgas)


#---------------------------------------------------------------------------
# To compare models
#---------------------------------------------------------------------------
# ===========================================================================
# ARIMA Model Selection with Error Handling and Residual Diagnostics
# ===========================================================================

# Load required libraries
library(forecast)
library(tseries)

# Fit the three competing models
fit_110 <- Arima(elecsales, order = c(1, 1, 0))  # ARIMA(1,1,0)
fit_011 <- Arima(elecsales, order = c(0, 1, 1))  # ARIMA(0,1,1)
fit_111 <- Arima(elecsales, order = c(1, 1, 1))  # ARIMA(1,1,1)

# Compare AIC and BIC values
model_comparison <- data.frame(
  Model = c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)"),
  AIC = c(fit_110$aic, fit_011$aic, fit_111$aic),
  BIC = c(fit_110$bic, fit_011$bic, fit_111$bic)
)

# Print the comparison table
cat("=== Model Comparison (Lower AIC/BIC is Better) ===\n")
print(model_comparison)

# Determine the best model based on AIC
best_model_index <- which.min(model_comparison$AIC)
best_model_name <- model_comparison$Model[best_model_index]
best_model <- list(fit_110, fit_011, fit_111)[[best_model_index]]

# Explain the best model choice
cat("\n=== Best Model Selection ===\n")
cat("The best model is", best_model_name, "because it has the lowest AIC (", 
    model_comparison$AIC[best_model_index], ") and BIC (", 
    model_comparison$BIC[best_model_index], ").\n")
cat("AIC/BIC penalize model complexity, so lower values indicate a better trade-off between fit and simplicity.\n")

# Check residuals for the best model
cat("\n=== Residual Diagnostics for", best_model_name, "===\n")
checkresiduals(best_model)

# Ljung-Box test for residual autocorrelation (H0: residuals are white noise)
lb_test <- Box.test(residuals(best_model), type = "Ljung-Box", lag = 12)
cat("\nLjung-Box Test p-value:", lb_test$p.value, "\n")
if (lb_test$p.value > 0.05) {
  cat("GOOD: Residuals are white noise (no autocorrelation).\n")
} else {
  cat("WARNING: Residuals show significant autocorrelation. Consider a more complex model.\n")
}

# Summary of the best model
cat("\n=== Best Model Summary ===\n")
print(summary(best_model))

# Additional explanation
if (best_model_name == "ARIMA(1,1,0)") {
  cat("\nNOTE: ARIMA(1,1,0) implies the data depends on its immediate past value (AR term).\n")
} else if (best_model_name == "ARIMA(0,1,1)") {
  cat("\nNOTE: ARIMA(0,1,1) suggests shocks persist via the MA term (short-term memory).\n")
} else {
  cat("\nNOTE: ARIMA(1,1,1) combines AR and MA effects, capturing both past values and shocks.\n")
}