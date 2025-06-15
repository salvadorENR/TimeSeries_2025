## 9. Pronóstico en el 5 % final ---------------------------------------
library(forecast)

# Si train/test ya existen, se usan; si no, se recrean
if (!exists("ts_train")) {
  n_total <- length(ts_alumnos)
  cut     <- floor(0.95 * n_total)
  ts_train <- window(ts_alumnos, end   = time(ts_alumnos)[cut])
  ts_test  <- window(ts_alumnos, start = time(ts_alumnos)[cut] + 1)
}

fit_train <- Arima(ts_train, order = c(0, 1, 1), include.drift = TRUE)
fc        <- forecast(fit_train, h = length(ts_test))

accuracy(fc, ts_test)

autoplot(ts_train, series = "Entrenamiento") +
  autolayer(fc$mean, series = "Pronóstico") +
  autolayer(ts_test,   series = "Real") +
  autolayer(fc$upper[,2], series = "IC 95 %",  linetype = "dashed") +
  autolayer(fc$lower[,2], linetype = "dashed") +
  labs(title = "Pronóstico sobre el conjunto de validación",
       x = "Año", y = "Alumnos (millones)") +
  theme_minimal()
