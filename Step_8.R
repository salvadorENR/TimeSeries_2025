## 8. Diagnóstico de residuos ------------------------------------------
library(FinTS)

fit_ma11 <- readRDS("modelo_arima_0_1_1_drift.rds")
resid_ma11 <- residuals(fit_ma11)

ljung_p <- Box.test(resid_ma11, lag = 10, type = "Ljung-Box",
                    fitdf = length(fit_ma11$coef))$p.value
arch_p  <- ArchTest(resid_ma11, lags = 12)$p.value
cat("Ljung-Box p =", ljung_p, "\n")
cat("ARCH p      =", arch_p,  "\n")

par(mfrow = c(1, 2))
hist(resid_ma11, main = "Histograma de residuos", xlab = "")
qqnorm(resid_ma11); qqline(resid_ma11, col = "red")
par(mfrow = c(1, 1))
