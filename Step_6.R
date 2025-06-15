## 6. Ajuste de modelos candidatos -------------------------------------
library(forecast)

fit_ma11 <- Arima(ts_alumnos, order = c(0, 1, 1), include.drift = TRUE)
fit_ar11 <- Arima(ts_alumnos, order = c(1, 1, 0), include.drift = TRUE)

saveRDS(fit_ma11, "modelo_arima_0_1_1_drift.rds")
saveRDS(fit_ar11, "modelo_arima_1_1_0_drift.rds")
