library(readr)   # read_lines
library(dplyr)   # mutate, arrange, select
library(stringr) # str_remove_all, str_sub
library(ggplot2)
library(scales)

archivo <- "datos_educacion_primaria.csv"

# 1) Leer todas las líneas
lineas <- read_lines(archivo, skip_empty_rows = TRUE)

# 2) Quitar la cabecera y crear un tibble
datos <- tibble(raw = lineas[-1]) |>      # descarta la primera línea de encabezado
  mutate(
    sin_comillas = str_remove_all(raw, '"'),   # fuera comillas
    sin_comas    = str_remove_all(sin_comillas, ","),       # fuera separadores de miles
    Anio         = as.integer(str_sub(sin_comas, 1, 4)),    # primeros 4 dígitos
    Alumnos_millones = as.numeric(str_sub(sin_comas, 5))    # resto de la cadena
  ) |>
  select(Anio, Alumnos_millones) |>
  arrange(Anio)

# --- Comprobación rápida ----------------------------------------
print(head(datos, 3))
# A tibble: 3 × 2
#    Anio Alumnos_millones
#   <int>            <dbl>
# 1  1970             401.
# 2  1971             412.
# 3  1972             425.

# --- División 95 %-5 % ------------------------------------------
cut   <- floor(0.95 * nrow(datos))
train <- datos[1:cut, ]
test  <- datos[(cut + 1):nrow(datos), ]

# --- Gráfica con tryCatch ---------------------------------------
tryCatch(
  expr = {
    p <- ggplot() +
      geom_line(data = train,
                aes(x = Anio, y = Alumnos_millones, color = "Entrenamiento"),
                linewidth = 1) +
      geom_line(data = test,
                aes(x = Anio, y = Alumnos_millones, color = "Prueba"),
                linewidth = 1.5) +
      scale_color_manual(values = c("Entrenamiento" = "blue",
                                    "Prueba"        = "red"),
                         name = "") +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      labs(title = "División 95 %-5 %",
           x = "Año",
           y = "Alumnos (millones)") +
      theme_minimal()
    print(p)
  },
  error = function(e) {
    message("ggplot2 falló: ", e$message, "\nSe usará la gráfica base.")
    plot(train$Anio, train$Alumnos_millones,
         type = "l", col = "blue", lwd = 1,
         xlab = "Año", ylab = "Alumnos (millones)",
         main = "División 95 %-5 %")
    lines(test$Anio, test$Alumnos_millones, col = "red", lwd = 1.5)
    legend("topleft", legend = c("Entrenamiento", "Prueba"),
           col = c("blue", "red"), lwd = c(1, 1.5), bty = "n")
  }
)
