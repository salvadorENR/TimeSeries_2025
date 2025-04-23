# Cargar librerías
library(ggplot2)
library(dplyr)

# Crear una secuencia de tiempo
t <- seq(0, 2*pi, length.out = 100)

# Crear data frame con diferentes amplitudes
data <- data.frame(
  t = rep(t, 4),
  A = factor(rep(c(1, 2, 3, 4), each = length(t))),
  y = c(1*sin(t), 2*sin(t), 3*sin(t), 4*sin(t))
)

# Graficar
ggplot(data, aes(x = t, y = y, color = A)) +
  geom_line(size = 1.2) +
  labs(title = "Efecto de la amplitud (A) en una función armónica",
       x = "Tiempo (t)", y = "Valor", color = "Amplitud A") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))
