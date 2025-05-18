#---------------------------------------------------------------------------
# 1. Plots y carga de datos
#---------------------------------------------------------------------------

# Cargar bibliotecas necesarias
library(forecast)    # Para funciones de series de tiempo
library(ggplot2)     # Para gráficos
library(datasets)    # Para datasets integrados como Nile, LakeHuron
library(fpp2)        # Para elecsales, ausbeer, nottem
library(ukgasapi)    # Para el dataset UKgas
library(gridExtra)   # Para organizar múltiples gráficos

# Función para analizar una serie de tiempo
analyze_series <- function(data, name) {
  # Paso 1: Graficar la serie original
  p1 <- autoplot(data) + 
    ggtitle(paste("Original Series:", name)) +
    ylab("Value") +
    xlab("Time")
  print(p1)
  
  # Paso 2: Determinar si necesita diferenciación (sólo regular)
  ndiffs_result <- ndiffs(data)
  if (ndiffs_result > 0) {
    diff_data <- diff(data, differences = ndiffs_result)
    transformation <- paste("Applied", ndiffs_result, "order differencing")
  } else {
    diff_data <- data
    transformation <- "No differencing needed"
  }
  
  # Paso 3: Graficar ACF y PACF
  p2 <- ggAcf(diff_data) + ggtitle(paste("ACF of", name, "-", transformation))
  p3 <- ggPacf(diff_data) + ggtitle(paste("PACF of", name, "-", transformation))
  print(p2)
  print(p3)
  
  # Paso 4: Sugerencia de modelo
  cat("\n=== Analysis for", name, "===\n")
  cat("Transformation:", transformation, "\n")
  suggest_model(diff_data)
}

# Función auxiliar para sugerir modelos
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

# Lista de series
data_list <- list(
  elecsales = elecsales,
  ausbeer = ausbeer,
  Nile = Nile,
  nottem = nottem,
  LakeHuron = LakeHuron,
  UKgas = UKgas
)

# Analizar cada serie
for (i in seq_along(data_list)) {
  analyze_series(data_list[[i]], names(data_list)[i])
  if (i < length(data_list)) {
    readline(prompt = "Press [enter] to continue to next dataset...")
  }
}

#---------------------------------------------------------------------------
# 2. Diferenciación y graficación manual
#---------------------------------------------------------------------------

# elecsales: diferenciación de orden 1
elecsales_diff <- diff(elecsales, differences = 1)
autoplot(elecsales_diff) +
  ggtitle("elecsales - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# ausbeer: diferencia regular y estacional (lag=4)
ausbeer_diff <- diff(diff(ausbeer, lag = 4), differences = 1)
autoplot(ausbeer_diff) +
  ggtitle("ausbeer - Serie diferenciada (regular + estacional)") +
  xlab("Tiempo") + ylab("Diferencia")

# Nile: diferenciación de orden 1
Nile_diff <- diff(Nile, differences = 1)
autoplot(Nile_diff) +
  ggtitle("Nile - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# Aplicar diferenciación estacional (lag = 12)
nottem_diff <- diff(diff(nottem, lag = 12))
autoplot(nottem_diff) +
  ggtitle("nottem - Serie diferenciada (estacional)") +
  ylab("Diferencia") +
  xlab("Tiempo")

# LakeHuron: diferenciación de orden 1
LakeHuron_diff <- diff(LakeHuron, differences = 1)
autoplot(LakeHuron_diff) +
  ggtitle("LakeHuron - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

# UKgas: diferencia regular y estacional (lag=4)
UKgas_diff <- diff(diff(UKgas, lag = 4), differences = 1)
autoplot(UKgas_diff) +
  ggtitle("UKgas - Serie diferenciada (regular + estacional)") +
  xlab("Tiempo") + ylab("Diferencia")

#---------------------------------------------------------------------------
# 3. ACF y PACF de series diferenciadas
#---------------------------------------------------------------------------

ggAcf(elecsales_diff) + ggtitle("ACF - elecsales (diferenciada)")
ggPacf(elecsales_diff) + ggtitle("PACF - elecsales (diferenciada)")

ggAcf(ausbeer_diff) + ggtitle("ACF - ausbeer (diferenciada)")
ggPacf(ausbeer_diff) + ggtitle("PACF - ausbeer (diferenciada)")

ggAcf(Nile_diff) + ggtitle("ACF - Nile (diferenciada)")
ggPacf(Nile_diff) + ggtitle("PACF - Nile (diferenciada)")

ggAcf(nottem) + ggtitle("ACF - nottem (serie original)")
ggPacf(nottem) + ggtitle("PACF - nottem (serie original)")

ggAcf(LakeHuron_diff) + ggtitle("ACF - LakeHuron (diferenciada)")
ggPacf(LakeHuron_diff) + ggtitle("PACF - LakeHuron (diferenciada)")

ggAcf(UKgas_diff) + ggtitle("ACF - UKgas (diferenciada)")
ggPacf(UKgas_diff) + ggtitle("PACF - UKgas (diferenciada)")

#---------------------------------------------------------------------------
# 4. Modelos automáticos con auto.arima()
#---------------------------------------------------------------------------

auto.arima(elecsales)
auto.arima(ausbeer)
auto.arima(ausbeer_diff)
auto.arima(Nile)
auto.arima(nottem)
auto.arima(LakeHuron)
auto.arima(UKgas)
