## 1. División de datos -------------------------------------------------
# 1.1 Librerías
library(readr); library(dplyr); library(stringr)

# 1.2 Carga y limpieza del CSV
archivo <- "datos_educacion_primaria.csv"
lineas  <- read_lines(archivo, skip_empty_rows = TRUE)

datos <- tibble(raw = lineas[-1]) |>
  mutate(
    sin_comillas = str_remove_all(raw, '"'),
    sin_comas    = str_remove_all(sin_comillas, ","),
    Anio         = as.integer(str_sub(sin_comas, 1, 4)),
    Alumnos_millones = as.numeric(str_sub(sin_comas, 5))
  ) |>
  arrange(Anio)

# 1.3 Conversión a serie ts anual
ts_alumnos <- ts(datos$Alumnos_millones,
                 start = min(datos$Anio),
                 frequency = 1)

# 1.4 Corte 95 %–5 %
n_total <- length(ts_alumnos)
cut     <- floor(0.95 * n_total)
ts_train <- window(ts_alumnos, end   = time(ts_alumnos)[cut])
ts_test  <- window(ts_alumnos, start = time(ts_alumnos)[cut] + 1)
