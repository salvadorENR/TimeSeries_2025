# Paso 0: Instalar y cargar paquetes necesarios
packages <- c("forecast", "tseries", "urca", "ggplot2")
installed <- packages %in% installed.packages()
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

# Paso 1: Cargar la serie de tiempo
data("AirPassengers")  # Serie mensual 1949–1960
z <- log(AirPassengers)  # Se aplica log para estabilizar varianza

# Paso 2: Verificar estacionariedad
adf.test(z)          # Prueba ADF
pp.test(z)           # Prueba Phillips-Perron
kpss.test(z)         # Prueba KPSS

# Paso 3: Transformar la serie si no es estacionaria
# Ya aplicamos log; ahora diferenciamos si es necesario
dz <- diff(z, differences = 1)
adf.test(dz)

# Paso 4: Ajustar modelo ARIMA automáticamente
fit <- auto.arima(z)
summary(fit)

# Paso 5: Diagnóstico de residuos
Box.test(residuals(fit), lag = 20, type = "Ljung-Box")  # Independencia
shapiro.test(residuals(fit))                           # Normalidad

# Paso 6: Verosimilitud, AIC y BIC
logLik(fit)
AIC(fit)
BIC(fit)

# Paso 7: Realizar predicción
forecast_result <- forecast(fit, h = 12)  # Pronóstico 12 pasos adelante
plot(forecast_result)
