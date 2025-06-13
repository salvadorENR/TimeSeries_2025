# ================================================================
# 0. Librerías ----------------------------------------------------
# ================================================================
library(readr)      # lectura de archivos
library(dplyr)      # manipulación de datos
library(stringr)    # funciones de texto
library(ggplot2)    # gráficas
library(scales)     # etiquetas numéricas

# ================================================================
# 1. Cargar y limpiar el archivo ---------------------------------
#    (formato: todo entre comillas y con separadores de miles)
# ================================================================
archivo <- "datos_educacion_primaria.csv"

lineas <- read_lines(archivo, skip_empty_rows = TRUE)

datos <- tibble(raw = lineas[-1]) |>                 # descarta la cabecera
  mutate(
    sin_comillas = str_remove_all(raw, '"'),         # quita comillas
    sin_comas    = str_remove_all(sin_comillas, ","),# quita separador de miles
    Anio         = as.integer(str_sub(sin_comas, 1, 4)),
    Alumnos_millones = as.numeric(str_sub(sin_comas, 5))
  ) |>
  select(Anio, Alumnos_millones) |>
  arrange(Anio)

# ================================================================
# 2. Estadísticos descriptivos -----------------------------------
# ================================================================
print(summary(datos$Alumnos_millones))

datos |> 
  summarise(
    n           = n(),
    media       = mean(Alumnos_millones, na.rm = TRUE),
    mediana     = median(Alumnos_millones, na.rm = TRUE),
    sd          = sd(Alumnos_millones, na.rm = TRUE),
    minimo      = min(Alumnos_millones, na.rm = TRUE),
    maximo      = max(Alumnos_millones, na.rm = TRUE)
  ) |>
  print()

# ================================================================
# 3. Evolución anual (diferencia y % de crecimiento) -------------
# ================================================================
datos <- datos |>
  mutate(
    delta_abs  = Alumnos_millones - lag(Alumnos_millones),
    delta_pct  = 100 * delta_abs / lag(Alumnos_millones)
  )

# ================================================================
# 4. Gráficas exploratorias --------------------------------------
# ================================================================
# 4.1 Serie temporal completa
ggplot(datos, aes(x = Anio, y = Alumnos_millones)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Alumnos matriculados (millones) por año",
       x = "Anio",
       y = "Alumnos (millones)") +
  theme_minimal()

# 4.2 Crecimiento porcentual anual
ggplot(datos, aes(x = Anio, y = delta_pct)) +
  geom_col(fill = "firebrick") +
  labs(title = "Crecimiento porcentual anual",
       x = "Anio",
       y = "% de crecimiento") +
  theme_minimal()

# 4.3 Histograma de valores absolutos
ggplot(datos, aes(x = Alumnos_millones)) +
  geom_histogram(color = "white", fill = "darkgreen", bins = 15) +
  labs(title = "Distribución de alumnos (millones)",
       x = "Alumnos (millones)",
       y = "Frecuencia") +
  theme_minimal()

# ================================================================
# 5. Modelo lineal simple (tendencia) ----------------------------
# ================================================================
ajuste <- lm(Alumnos_millones ~ Anio, data = datos)
print(summary(ajuste))

# Añadir la recta de tendencia a la serie temporal
ggplot(datos, aes(x = Anio, y = Alumnos_millones)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Serie temporal con tendencia lineal",
       x = "Anio",
       y = "Alumnos (millones)") +
  theme_minimal()

# ================================================================
# 6. División 95 %-5 % (si se requiere para validación) ----------
# ================================================================
cut   <- floor(0.95 * nrow(datos))
train <- datos[1:cut, ]
test  <- datos[(cut + 1):nrow(datos), ]

ggplot() +
  geom_line(data = train, aes(x = Anio, y = Alumnos_millones, color = "Entrenamiento"), linewidth = 1) +
  geom_line(data = test,  aes(x = Anio, y = Alumnos_millones, color = "Prueba"),        linewidth = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "blue", "Prueba" = "red"), name = "") +
  labs(title = "División 95 %-5 %",
       x = "Anio",
       y = "Alumnos (millones)") +
  theme_minimal()

# ================================================================
# Fin del análisis exploratorio
# ================================================================
