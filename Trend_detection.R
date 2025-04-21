# =============================
# LIBRERÍAS NECESARIAS
# =============================
if (!require("trend")) install.packages("trend")
library(trend)
library(forecast)

# =============================
# SERIE TEMPORAL DE EJEMPLO
# =============================
set.seed(123)
valores <- 10 + 2 * (1:20) + rnorm(20, mean = 0, sd = 2)

# ✅ Establecer frecuencia (trimestral)
serie <- ts(valores, frequency = 4)

# =============================
# 1. GRÁFICO VISUAL
# =============================
plot(serie, main = "Gráfico de la serie temporal", col = "blue", lwd = 2)

# =============================
# 2. DESCOMPOSICIÓN
# =============================
modelo <- decompose(serie, type = "additive")
plot(modelo$trend, main = "Componente de tendencia")

# Analizar la componente de tendencia
if (all(is.na(modelo$trend))) {
  cat("\n[Descomposición] No se pudo calcular la tendencia (demasiados NA).\n")
} else if (var(na.omit(modelo$trend)) < 1e-6) {
  cat("\n[Descomposición] No se detecta una tendencia significativa en la serie.\n")
} else {
  cat("\n[Descomposición] Se observa una tendencia en la serie (la componente de tendencia no es constante).\n")
}

# =============================
# 3. REGRESIÓN LINEAL
# =============================
t <- 1:length(serie)
modelo_lm <- lm(serie ~ t)
resumen_lm <- summary(modelo_lm)

# Coeficiente y valor-p
coef_t <- resumen_lm$coefficients[2, 1]
pval_t <- resumen_lm$coefficients[2, 4]

cat("\n[Regresión lineal] Coeficiente de t:", round(coef_t, 4), "\n")
cat("[Regresión lineal] Valor-p:", round(pval_t, 4), "\n")

if (pval_t < 0.05) {
  cat("[Regresión lineal] Se detecta una tendencia lineal significativa.\n")
} else {
  cat("[Regresión lineal] No hay evidencia suficiente para afirmar que existe una tendencia lineal.\n")
}

# =============================
# 4. PRUEBA DE MANN-KENDALL
# =============================
mk_resultado <- mk.test(serie)

cat("\n[Mann-Kendall] Estadístico Z:", round(mk_resultado$statistic, 4), "\n")
cat("[Mann-Kendall] Valor-p:", round(mk_resultado$p.value, 4), "\n")

if (mk_resultado$p.value < 0.05) {
  cat("[Mann-Kendall] Se detecta una tendencia monotónica significativa en la serie.\n")
} else {
  cat("[Mann-Kendall] No se detecta una tendencia significativa en la serie.\n")
}
