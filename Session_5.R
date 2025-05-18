# Establecer semilla para reproducibilidad
set.seed (123)
# 1. Simular una serie AR(1) con phi = -0.9 y 1000 observaciones
serie <- arima.sim(n = 1000, model = list(ar = -0.9))
# 2. Graficar la serie temporal
plot(serie , type = "l", main = expression("Serie AR(1) con "
                                           * phi == -0.9),
     ylab = expression(z[t]), xlab = "Tiempo")
# 3. F u n c i n de autocorrelaci n (ACF o FAS)
acf(serie , lag.max = 30, main = "ACF (FAS) de la serie AR(1)
con
= -0.9")
# 4. F u n c i n de autocorrelaci n parcial (PACF)
pacf(serie , lag.max = 30, main = "PACF de la serie AR(1) con
= -0.9")
# 5. Ajustar modelo ARIMA y revisar coeficientes
modelo<-arima(serie,order=c(1,0,0))
summary(modelo)
