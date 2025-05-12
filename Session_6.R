set.seed (123) # Para reproducibilidad
#Simular un MA(1) con coeficiente ma1 = 0.6 y 200 observaciones
ma1_sim <- arima.sim(n = 200, model = list(ma = 0.6))
#Graficar la serie simulada
plot(ma1_sim , main = " Simulación de un MA(1)", ylab = "
Valor", col = "steelblue")
#Graficar la ACF para verificar el comportamiento típico de un MA(1)
acf(ma1_sim , main = "ACF de la serie MA(1) simulada")
#Graficar la PACF
pacf(ma1_sim , main = "PACF de la serie MA(1) simulada")
