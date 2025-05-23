# Cargar las bibliotecas necesarias y los conjuntos de datos
library(forecast)    # Para funciones de series de tiempo
library(ggplot2)     # Para gráficos
library(datasets)    # Para los conjuntos de datos Nile, LakeHuron
library(fpp2)        # Para elecsales, ausbeer, nottem
library(ukgasapi)    # Para el conjunto de datos UKgas
library(gridExtra)   # Para organizar los gráficos

#--------------------------------------------------------------
# 1. ELECSALES
#--------------------------------------------------------------
autoplot(elecsales) +
  ggtitle("Serie original - elecsales") +
  xlab("Tiempo") + ylab("Valor")

elecsales_diff <- diff(elecsales, differences = 1)

autoplot(elecsales_diff) +
  ggtitle("elecsales - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

gridExtra::grid.arrange(
  ggAcf(elecsales_diff) + ggtitle("ACF - elecsales (diferenciada)"),
  ggPacf(elecsales_diff) + ggtitle("PACF - elecsales (diferenciada)"),
  ncol = 1
)

Arima(elecsales, c(1, 1, 0))
auto.arima(elecsales)

#--------------------------------------------------------------
# 2. AUSBEER
#--------------------------------------------------------------
autoplot(ausbeer) +
  ggtitle("Serie original - ausbeer") +
  xlab("Tiempo") + ylab("Valor")

ausbeer_diff <- diff(diff(ausbeer, lag = 12), differences = 1)

autoplot(ausbeer_diff) +
  ggtitle("ausbeer - Serie diferenciada (regular + estacional)") +
  xlab("Tiempo") + ylab("Diferencia")

gridExtra::grid.arrange(
  ggAcf(ausbeer_diff) + ggtitle("ACF - ausbeer (diferenciada)"),
  ggPacf(ausbeer_diff) + ggtitle("PACF - ausbeer (diferenciada)"),
  ncol = 1
)

model_1 <- Arima(ausbeer, order = c(1, 1, 1), 
                 seasonal = list(order = c(0, 1, 1), period = 12))
summary(model_1)

model_2 <- auto.arima(ausbeer)
summary(model_2)

aic_1 <- AIC(model_1); bic_1 <- BIC(model_1)
aic_2 <- AIC(model_2); bic_2 <- BIC(model_2)

comparison <- data.frame(Model = c("model_1", "model_2"), AIC = c(aic_1, aic_2), BIC = c(bic_1, bic_2))
cat("=== Comparación de Modelos ===\n"); print(comparison)

if (aic_1 < aic_2) {
  cat("\nSegún AIC, el mejor modelo es: model_1\n")
} else if (aic_2 < aic_1) {
  cat("\nSegún AIC, el mejor modelo es: model_2\n")
} else {
  cat("\nAmbos modelos tienen el mismo AIC.\n")
}

if (bic_1 < bic_2) {
  cat("Según BIC, el mejor modelo es: model_1\n")
} else if (bic_2 < bic_1) {
  cat("Según BIC, el mejor modelo es: model_2\n")
} else {
  cat("Ambos modelos tienen el mismo BIC.\n")
}

#--------------------------------------------------------------
# 3. NILE
#--------------------------------------------------------------
autoplot(Nile) +
  ggtitle("Serie original - Nile") +
  xlab("Tiempo") + ylab("Valor")

Nile_diff <- diff(Nile, differences = 1)

autoplot(Nile_diff) +
  ggtitle("Nile - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

gridExtra::grid.arrange(
  ggAcf(Nile_diff) + ggtitle("ACF - Nile (diferenciada)"),
  ggPacf(Nile_diff) + ggtitle("PACF - Nile (diferenciada)"),
  ncol = 1
)

model_1 <- Arima(Nile, order = c(1, 1, 1))
summary(model_1)

model_2 <- auto.arima(Nile)
summary(model_2)

aic_1 <- AIC(model_1); bic_1 <- BIC(model_1)
aic_2 <- AIC(model_2); bic_2 <- BIC(model_2)

comparison <- data.frame(Model = c("model_1", "model_2"), AIC = c(aic_1, aic_2), BIC = c(bic_1, bic_2))
cat("=== Comparación de Modelos ===\n"); print(comparison)

if (aic_1 < aic_2) {
  cat("\nSegún AIC, el mejor modelo es: model_1\n")
} else if (aic_2 < aic_1) {
  cat("\nSegún AIC, el mejor modelo es: model_2\n")
} else {
  cat("\nAmbos modelos tienen el mismo AIC.\n")
}

if (bic_1 < bic_2) {
  cat("Según BIC, el mejor modelo es: model_1\n")
} else if (bic_2 < bic_1) {
  cat("Según BIC, el mejor modelo es: model_2\n")
} else {
  cat("Ambos modelos tienen el mismo BIC.\n")
}

#--------------------------------------------------------------
# 4. NOTTEM
#--------------------------------------------------------------
autoplot(nottem) +
  ggtitle("Serie original - nottem") +
  xlab("Tiempo") + ylab("Valor")

nottem_diff <- diff(diff(nottem, lag = 12))

autoplot(nottem_diff) +
  ggtitle("nottem - Serie diferenciada (estacional)") +
  xlab("Tiempo") + ylab("Diferencia")

gridExtra::grid.arrange(
  ggAcf(nottem_diff) + ggtitle("ACF - nottem (diferenciada)"),
  ggPacf(nottem_diff) + ggtitle("PACF - nottem (diferenciada)"),
  ncol = 1
)

model_1 <- Arima(nottem, order = c(1, 0, 1), 
                 seasonal = list(order = c(1, 1, 2), period = 12))
summary(model_1)

model_2 <- auto.arima(nottem)
summary(model_2)

aic_1 <- AIC(model_1); bic_1 <- BIC(model_1)
aic_2 <- AIC(model_2); bic_2 <- BIC(model_2)

comparison <- data.frame(Model = c("model_1", "model_2"), AIC = c(aic_1, aic_2), BIC = c(bic_1, bic_2))
cat("=== Comparación de Modelos ===\n"); print(comparison)

if (aic_1 < aic_2) {
  cat("\nSegún AIC, el mejor modelo es: model_1\n")
} else if (aic_2 < aic_1) {
  cat("\nSegún AIC, el mejor modelo es: model_2\n")
} else {
  cat("\nAmbos modelos tienen el mismo AIC.\n")
}

if (bic_1 < bic_2) {
  cat("Según BIC, el mejor modelo es: model_1\n")
} else if (bic_2 < bic_1) {
  cat("Según BIC, el mejor modelo es: model_2\n")
} else {
  cat("Ambos modelos tienen el mismo BIC.\n")
}

#--------------------------------------------------------------
# 5. LAKEHURON
#--------------------------------------------------------------
autoplot(LakeHuron) +
  ggtitle("Serie original - LakeHuron") +
  xlab("Tiempo") + ylab("Valor")

LakeHuron_diff <- diff(LakeHuron, differences = 1)

autoplot(LakeHuron_diff) +
  ggtitle("LakeHuron - Serie diferenciada (orden 1)") +
  xlab("Tiempo") + ylab("Diferencia")

gridExtra::grid.arrange(
  ggAcf(LakeHuron_diff) + ggtitle("ACF - LakeHuron (diferenciada)"),
  ggPacf(LakeHuron_diff) + ggtitle("PACF - LakeHuron (diferenciada)"),
  ncol = 1
)

model_1 <- Arima(LakeHuron, order = c(0, 1, 0))
summary(model_1)

model_2 <- auto.arima(LakeHuron)
summary(model_2)

aic_1 <- AIC(model_1); bic_1 <- BIC(model_1)
aic_2 <- AIC(model_2); bic_2 <- BIC(model_2)

comparison <- data.frame(Model = c("model_1", "model_2"), AIC = c(aic_1, aic_2), BIC = c(bic_1, bic_2))
cat("=== Comparación de Modelos ===\n"); print(comparison)

if (aic_1 < aic_2) {
  cat("\nSegún AIC, el mejor modelo es: model_1\n")
} else if (aic_2 < aic_1) {
  cat("\nSegún AIC, el mejor modelo es: model_2\n")
} else {
  cat("\nAmbos modelos tienen el mismo AIC.\n")
}

if (bic_1 < bic_2) {
  cat("Según BIC, el mejor modelo es: model_1\n")
} else if (bic_2 < bic_1) {
  cat("Según BIC, el mejor modelo es: model_2\n")
} else {
  cat("Ambos modelos tienen el mismo BIC.\n")
}

#--------------------------------------------------------------
# 6. UKGAS
#--------------------------------------------------------------
autoplot(UKgas) +
  ggtitle("Serie original - UKgas") +
  xlab("Tiempo") + ylab("Valor")

UKgas_diff <- diff(diff(UKgas, lag = 12), differences = 1)

autoplot(UKgas_diff) +
  ggtitle("UKgas - Serie diferenciada (regular + estacional)") +
  xlab("Tiempo") + ylab("Diferencia")

gridExtra::grid.arrange(
  ggAcf(UKgas_diff) + ggtitle("ACF - UKgas (diferenciada)"),
  ggPacf(UKgas_diff) + ggtitle("PACF - UKgas (diferenciada)"),
  ncol = 1
)

model_1 <- Arima(UKgas, order = c(0, 1, 0), 
                 seasonal = list(order = c(0, 1, 1), period = 2))
summary(model_1)

model_2 <- auto.arima(UKgas)
summary(model_2)

aic_1 <- AIC(model_1); bic_1 <- BIC(model_1)
aic_2 <- AIC(model_2); bic_2 <- BIC(model_2)

comparison <- data.frame(Model = c("model_1", "model_2"), AIC = c(aic_1, aic_2), BIC = c(bic_1, bic_2))
cat("=== Comparación de Modelos ===\n"); print(comparison)

if (aic_1 < aic_2) {
  cat("\nSegún AIC, el mejor modelo es: model_1\n")
} else if (aic_2 < aic_1) {
  cat("\nSegún AIC, el mejor modelo es: model_2\n")
} else {
  cat("\nAmbos modelos tienen el mismo AIC.\n")
}

if (bic_1 < bic_2) {
  cat("Según BIC, el mejor modelo es: model_1\n")
} else if (bic_2 < bic_1) {
  cat("Según BIC, el mejor modelo es: model_2\n")
} else {
  cat("Ambos modelos tienen el mismo BIC.\n")
}
