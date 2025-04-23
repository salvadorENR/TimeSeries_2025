# 1. Simular la serie como la que vimos en Python
set.seed(42)
t <- 1:100
mu <- 10
A1 <- 3
A2 <- 1.5
omega1 <- 2 * pi / 12
omega2 <- 2 * pi / 6
theta1 <- 0
theta2 <- pi / 4

z <- mu + A1 * sin(omega1 * t + theta1) + A2 * cos(omega2 * t + theta2) + rnorm(length(t), mean = 0, sd = 0.5)

# 2. Crear las variables armónicas
harmonicos <- data.frame(
  t = t,
  y = z,
  sin1 = sin(omega1 * t),
  cos1 = cos(omega1 * t),
  sin2 = sin(omega2 * t),
  cos2 = cos(omega2 * t)
)

# 3. Ajustar el modelo lineal
modelo <- lm(y ~ sin1 + cos1 + sin2 + cos2, data = harmonicos)

# 4. Ver el resumen del modelo
summary(modelo)

# 5. Graficar el ajuste
ajuste <- ts(fitted(modelo))
serie <- ts(z)

plot(serie, type = "l", col = "black", lwd = 2, main = "Ajuste del Modelo Armónico con dos frecuencias")
lines(ajuste, col = "blue", lwd = 2)
legend("topright", legend = c("Serie original", "Modelo ajustado"), col = c("black", "blue"), lty = 1)
