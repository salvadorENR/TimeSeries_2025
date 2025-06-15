## 7. Comparación AIC/BIC ----------------------------------------------
fit_ma11 <- readRDS("modelo_arima_0_1_1_drift.rds")
fit_ar11 <- readRDS("modelo_arima_1_1_0_drift.rds")

metricas <- data.frame(
  Modelo = c("ARIMA(0,1,1)+drift", "ARIMA(1,1,0)+drift"),
  AIC  = c(fit_ma11$aic,  fit_ar11$aic),
  AICc = c(fit_ma11$aicc, fit_ar11$aicc),
  BIC  = c(fit_ma11$bic,  fit_ar11$bic)
)
print(metricas)
