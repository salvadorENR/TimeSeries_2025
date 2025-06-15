## 5. ACF / PACF --------------------------------------------------------
par(mfrow = c(1, 2))
acf(ts_diff1, lag.max = 20, main = "ACF de ΔY")
pacf(ts_diff1, lag.max = 20, main = "PACF de ΔY")
par(mfrow = c(1, 1))
# Lectura: patrón sugiere MA(1) → ARIMA(0,1,1); se probará también ARIMA(1,1,0)
