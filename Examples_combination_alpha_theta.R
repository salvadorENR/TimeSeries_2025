# ====================================
# LIBRERÍAS
# ====================================
library(forecast)
library(ggplot2)
library(gridExtra)

# ====================================
# SERIE DE TIEMPO FICTICIA CON TENDENCIA
# ====================================
set.seed(123)
tiempo <- 1:20
valores <- 10 + 2 * tiempo + rnorm(20, mean = 0, sd = 2)
serie <- ts(valores)

# ====================================
# FUNCIÓN MODIFICADA CON tryCatch
# ====================================
graficar_holt <- function(serie, alpha, beta) {
  tryCatch({
    modelo <- holt(serie, alpha = alpha, beta = beta, h = 5, exponential = FALSE)
    autoplot(modelo) +
      ggtitle(paste("α =", alpha, "| β =", beta)) +
      ylab("Valor") + xlab("Tiempo") +
      theme_minimal()
  }, error = function(e) {
    ggplot() +
      annotate("text", x = 1, y = 1, label = paste("❌ Error:\nα =", alpha, "\nβ =", beta), size = 5) +
      theme_void()
  })
}

# ====================================
# COMBINACIONES DE PARÁMETROS
# ====================================
alphas <- c(0.2, 0.5, 0.8)
betas <- c(0.2, 0.5, 0.8)

plots <- list()
index <- 1

for (a in alphas) {
  for (b in betas) {
    plots[[index]] <- graficar_holt(serie, alpha = a, beta = b)
    index <- index + 1
  }
}

# ====================================
# MOSTRAR TODOS LOS GRÁFICOS JUNTOS
# ====================================
do.call("grid.arrange", c(plots, ncol = 3))
