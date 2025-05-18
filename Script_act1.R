#---------------------------------------------------------------------------
# 1. Plots
#---------------------------------------------------------------------------

# Load necessary libraries and datasets
library(forecast)    # For time series functions
library(ggplot2)     # For plotting
library(datasets)    # For Nile, LakeHuron
library(fpp2)        # For elecsales, ausbeer, nottem
library(ukgasapi)       # For UKgas dataset
library(ggplot2)   # Para los gráficos

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

#---------------------------------------------------------------------
# 2. Differentiation 
#---------------------------------------------------------------------

# === ELECSALES ===
elecsales_diff <- diff(elecsales, differences = 1)
autoplot(elecsales_diff) +
  ggtitle("elecsales - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# === AUSBEER ===
ausbeer_diff <- diff(diff(ausbeer, lag = 4), differences = 1)
autoplot(ausbeer_diff) +
  ggtitle("ausbeer - Serie diferenciada (regular + estacional)") +
  xlab("Tiempo") + ylab("Diferencia")

# === NILE ===
Nile_diff <- diff(Nile, differences = 1)
autoplot(Nile_diff) +
  ggtitle("Nile - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# === NOTTEM ===
nottem_diff <- diff(diff(nottem, lag = 12))
autoplot(nottem_diff) +
  ggtitle("nottem - Serie diferenciada (estacional)") +
  ylab("Diferencia") + xlab("Tiempo")

# === LAKEHURON ===
LakeHuron_diff <- diff(LakeHuron, differences = 1)
autoplot(LakeHuron_diff) +
  ggtitle("LakeHuron - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# === UKGAS ===
UKgas_diff <- diff(diff(UKgas, lag = 4), differences = 1)
autoplot(UKgas_diff) +
  ggtitle("UKgas - Serie diferenciada (regular + estacional)") +
  xlab("Tiempo") + ylab("Diferencia")


#-------------------------------------------------------------------------------
# 3. ACF and PACF
#-------------------------------------------------------------------------------

# Serie 1: elecsales (diferenciada)
elecsales_diff <- diff(elecsales, differences = 1)
ggAcf(elecsales_diff) + ggtitle("ACF - elecsales (diferenciada)")
ggPacf(elecsales_diff) + ggtitle("PACF - elecsales (diferenciada)")

# Serie 2: ausbeer (diferencia regular + estacional)
ausbeer_diff <- diff(diff(ausbeer, lag = 4), differences = 1)
ggAcf(ausbeer_diff) + ggtitle("ACF - ausbeer (diferenciada)")
ggPacf(ausbeer_diff) + ggtitle("PACF - ausbeer (diferenciada)")

# Serie 3: Nile (diferenciada)
Nile_diff <- diff(Nile, differences = 1)
ggAcf(Nile_diff) + ggtitle("ACF - Nile (diferenciada)")
ggPacf(Nile_diff) + ggtitle("PACF - Nile (diferenciada)")

# Serie 4: nottem (no diferenciada)
nottem_diff <- diff(diff(nottem, lag = 12))
ggAcf(nottem_diff) + ggtitle("ACF - nottem (diferenciada)")
ggPacf(nottem_diff) + ggtitle("PACF - nottem (diferenciada)")

# Serie 5: LakeHuron (diferenciada)
LakeHuron_diff <- diff(LakeHuron, differences = 1)
ggAcf(LakeHuron_diff) + ggtitle("ACF - LakeHuron (diferenciada)")
ggPacf(LakeHuron_diff) + ggtitle("PACF - LakeHuron (diferenciada)")

# Serie 6: UKgas (diferencia regular + estacional)
UKgas_diff <- diff(diff(UKgas, lag = 4), differences = 1)
ggAcf(UKgas_diff) + ggtitle("ACF - UKgas (diferenciada)")
ggPacf(UKgas_diff) + ggtitle("PACF - UKgas (diferenciada)")


auto.arima(elecsales)
auto.arima(ausbeer)
auto.arima(ausbeer_diff)

#------------------------------------------------------------------------------
# Automatic function of ARIMA
##------------------------------------------------------------------------------
auto.arima(elecsales)
auto.arima(ausbeer)
auto.arima(Nile)
auto.arima(nottem)
auto.arima(LakeHuron)
auto.arima(UKgas)






