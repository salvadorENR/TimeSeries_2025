## 3. Estacionariedad ---------------------------------------------------
library(tseries)

adf_nivel  <- adf.test(ts_alumnos, alternative = "stationary")
kpss_nivel <- kpss.test(ts_alumnos, null = "Level")

cat("ADF p =", adf_nivel$p.value, "\n")
cat("KPSS p =", kpss_nivel$p.value, "\n")
