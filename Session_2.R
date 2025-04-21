#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Parte I: Suavizamiento de Holt
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(forecast)

# Cargar la serie de población
pob <- read.csv("C:/Users/DELL/Documents/TimeSeries_2025/poblacion_SV.csv", header = FALSE)
y <- ts(pob, start = c(1960), frequency = 1)

# Aplicar modelo de Holt
holt_pob <- holt(y, alpha = 0.3, beta = 0.1, h = 5)
summary(holt_pob)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Parte II: Descomposición aditiva
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(forecast)

# Cargar la serie de pasajeros aéreos
pas <- read.csv("C:/Users/DELL/Documents/TimeSeries_2025/AirPassengers.csv")
z <- ts(pas[[2]], start = c(1949), frequency = 12)

# Graficar la serie original
plot(z)

# Descomponer la serie (corrección del error aquí)
modelo <- decompose(z, type = "additive")
plot(modelo)

# Extraer los componentes
modelo$trend
modelo$seasonal
modelo$random

# Reconstruir la serie
reconstruida <- modelo$trend + modelo$seasonal + modelo$random

# Graficar serie original vs reconstruida
plot(z, col = "black", lwd = 2,
     main = "Serie original vs reconstruida",
     ylab = "Valor", xlab = "Tiempo")

# Agregar la serie reconstruida
lines(reconstruida, col = "red", lwd = 2, lty = 2)

# Agregar leyenda
legend("topleft", legend = c("Original", "Reconstruida"),
       col = c("black", "red"), lwd = 2, lty = c(1, 2))
