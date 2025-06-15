## 4. Transformaciones / diferencias -----------------------------------
library(FinTS); library(forecast)

# 4.1 Primera diferencia
ts_diff1 <- diff(ts_alumnos)

# 4.2 Pruebas en ΔY
adf_diff1  <- adf.test(ts_diff1,  "stationary")
kpss_diff1 <- kpss.test(ts_diff1, "Level")
arch_diff1 <- ArchTest(ts_diff1, lags = 12)

# 4.3 (opcional) Segunda diferencia
ts_diff2   <- diff(ts_diff1)
adf_diff2  <- adf.test(ts_diff2,  "stationary")
kpss_diff2 <- kpss.test(ts_diff2, "Level")
