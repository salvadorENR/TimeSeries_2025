# =============================================
# SCRIPT 2: ANÁLISIS ARIMA CON ACF, PACF Y DIFERENCIACIÓN
# Proyecto de Investigación - Universidad de El Salvador
# =============================================

# 1. Cargar librerías ----
library(forecast)
library(tsibble)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(knitr)
library(ggfortify)

# 2. Cargar datos (del Script 1) ----
datos <- read_csv("datos_educacion_primaria.csv") %>%
  mutate(Año = yearmonth(paste(Año, "-01"))) %>%
  as_tsibble(index = Año)

# Convertir a objeto ts para análisis clásico
ts_original <- ts(datos$Alumnos_millones, start = 1970, frequency = 1)

# 3. Gráfico de la serie original ----
ggplot(datos, aes(x = Año, y = Alumnos_millones)) +
  geom_line(color = "#1E88E5", linewidth = 1.2) +
  geom_point(color = "#1E88E5", size = 1) +
  labs(title = "Matrícula en Educación Primaria (1970–2023)",
       subtitle = "Serie original en millones de alumnos",
       x = "Año", y = "Alumnos (millones)") +
  theme_minimal()

# 4. ACF y PACF de la serie original ----
par(mfrow = c(1, 2))
acf(ts_original, main = "ACF - Serie Original")
pacf(ts_original, main = "PACF - Serie Original")
par(mfrow = c(1, 1))

# =============================================
# ANÁLISIS DE DIFERENCIACIÓN
# =============================================

# 5. Primera diferenciación (d = 1) ----
ts_diff1 <- diff(ts_original)

# 6. Gráfico de la primera serie diferenciada ----
autoplot(ts_diff1) +
  labs(title = "Primera Diferenciación (d = 1)",
       x = "Año", y = "Diferencia de alumnos (millones)") +
  theme_minimal()

# 7. ACF y PACF de la primera serie diferenciada ----
par(mfrow = c(1, 2))
acf(ts_diff1, na.action = na.pass, main = "ACF - Primera Diferenciación (d=1)")
pacf(ts_diff1, na.action = na.pass, main = "PACF - Primera Diferenciación (d=1)")
par(mfrow = c(1, 1))

# 8. Segunda diferenciación (d = 2) ----
ts_diff2 <- diff(ts_diff1)

# 9. Gráfico de la segunda serie diferenciada ----
autoplot(ts_diff2) +
  labs(title = "Segunda Diferenciación (d = 2)",
       x = "Año", y = "Segunda diferencia de alumnos (millones)") +
  theme_minimal()

# 10. ACF y PACF de la segunda serie diferenciada ----
par(mfrow = c(1, 2))
acf(ts_diff2, na.action = na.pass, main = "ACF - Segunda Diferenciación (d=2)")
pacf(ts_diff2, na.action = na.pass, main = "PACF - Segunda Diferenciación (d=2)")
par(mfrow = c(1, 1))

# =============================================
# MODELADO ARIMA
# =============================================

# 11. División de datos (95% entrenamiento, 5% prueba) ----
n_total <- nrow(datos)
n_train <- round(n_total * 0.95)
train <- datos %>% slice(1:n_train)
test <- datos %>% slice((n_train + 1):n_total)

# 12. Modelado ARIMA ----
modelo <- auto.arima(train$Alumnos_millones,
                     seasonal = FALSE,
                     stepwise = FALSE,
                     approximation = FALSE)

# 13. Diagnóstico de residuos ----
checkresiduals(modelo)

# 14. Predicción ----
predicciones <- forecast(modelo, h = nrow(test))

# 15. Validación ----
accuracy(predicciones, test$Alumnos_millones) %>%
  kable(caption = "Métricas de Validación")

# 16. Crear carpeta 'resultados' si no existe ----
if (!dir.exists("resultados")) dir.create("resultados")

# 17. Guardar modelo y datos de prueba ----
saveRDS(modelo, "resultados/modelo_arima.rds")
write_csv(test, "resultados/datos_validacion.csv")