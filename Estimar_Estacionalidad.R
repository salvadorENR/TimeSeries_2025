# ==============================================
# Simulamos datos mensuales durante 3 años
# ==============================================
set.seed(123)
datos <- c(
  # Año 1
  20, 18, 22, 25, 27, 30, 29, 28, 26, 23, 21, 20,
  # Año 2
  21, 19, 23, 26, 28, 31, 30, 29, 27, 24, 22, 21,
  # Año 3
  22, 20, 24, 27, 29, 32, 31, 30, 28, 25, 23, 22
)

# Convertimos a serie temporal
serie <- ts(datos, frequency = 12, start = c(2020, 1))

# ==============================================
# Paso 1: Quitar la tendencia (simplificamos usando promedio)
# ==============================================
# Para este ejemplo, supondremos que ya se quitó la tendencia
# y usamos los datos directamente como la serie sin tendencia
E <- datos

# ==============================================
# Paso 2: Calcular promedio general
# ==============================================
E_barra <- mean(E)

# ==============================================
# Paso 3: Calcular el promedio de cada mes
# ==============================================
meses <- matrix(E, ncol = 12, byrow = TRUE)  # 3 filas (años), 12 columnas (meses)
promedios_mensuales <- colMeans(meses)

# ==============================================
# Paso 4: Calcular los efectos estacionales
# ==============================================
s_hat <- promedios_mensuales - E_barra

# ==============================================
# Mostrar resultados
# ==============================================
cat("Promedio general:", round(E_barra, 2), "\n\n")
cat("Efectos estacionales estimados (s_j):\n")
print(round(s_hat, 2))

cat("\nSuma de los s_j:", round(sum(s_hat), 10), "\n")
