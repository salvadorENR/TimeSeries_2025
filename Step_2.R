## 2. Análisis exploratorio --------------------------------------------
library(ggplot2); library(scales)

# 2.1 Estadísticos descriptivos
summary(datos$Alumnos_millones)

# 2.2 Serie temporal completa
ggplot(datos, aes(Anio, Alumnos_millones)) +
  geom_line(color = "steelblue") +
  labs(title = "Alumnos matriculados (millones)",
       x = "Año", y = "Alumnos (millones)") +
  theme_minimal()

# 2.3 Crecimientos absoluto y porcentual
datos <- datos |>
  mutate(
    delta_abs = Alumnos_millones - lag(Alumnos_millones),
    delta_pct = 100 * delta_abs / lag(Alumnos_millones)
  )
