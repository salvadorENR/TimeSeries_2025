# ================================================================
# 3. Pruebas de estacionariedad (ADF, KPSS)
#    Requiere tener ya el data-frame `datos` con las columnas
#    `Anio` y `Alumnos_millones` creado en el paso anterior.
# ================================================================

# ── 1. Instalar/cargar los paquetes necesarios ──────────────────
if (!requireNamespace("tseries", quietly = TRUE)) install.packages("tseries")
if (!requireNamespace("urca",   quietly = TRUE)) install.packages("urca")

library(tseries)   # adf.test(), kpss.test()
library(urca)      # ur.df() para ADF detallado si se desea

# ── 2. Crear la serie de tiempo (frecuencia = 1 porque es anual)─
ts_alumnos <- ts(
  datos$Alumnos_millones,
  start     = min(datos$Anio),
  frequency = 1
)

# ── 3. Prueba Dickey-Fuller aumentada (ADF) ---------------------
#     Hipótesis nula H0: la serie tiene raíz unitaria → no estacionaria
adf_out <- adf.test(ts_alumnos, alternative = "stationary")
print(adf_out)

## (opcional) versión ur.df con rezagos explícitos
# adf_ur <- ur.df(ts_alumnos, type = "trend", lags = 3)
# summary(adf_ur)

# ── 4. Prueba KPSS ----------------------------------------------
#     Hipótesis nula H0: la serie es estacionaria
kpss_out <- kpss.test(ts_alumnos, null = "Level")   # "Trend" si se analiza tendencia
print(kpss_out)

# ── 5. Interpretación rápida ------------------------------------
cat("\n--- Interpretación (significancia 5 %) ------------------\n")
cat(sprintf("ADF  p-value = %.4f  → %s H0 (raíz unitaria)\n",
            adf_out$p.value,
            if (adf_out$p.value < 0.05) "RECHAZAR" else "NO rechazar"))
cat(sprintf("KPSS p-value = %.4f  → %s H0 (estacionariedad)\n",
            kpss_out$p.value,
            if (kpss_out$p.value < 0.05) "RECHAZAR" else "NO rechazar"))
