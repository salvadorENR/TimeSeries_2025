# Load required package
if (!require("forecast")) install.packages("forecast", dependencies = TRUE)
library(forecast)

# Plot function
plot_acf_pacf <- function(series, main_title) {
  par(mfrow = c(1, 2))
  acf(series, main = paste("ACF -", main_title))
  pacf(series, main = paste("PACF -", main_title))
  par(mfrow = c(1, 1))
}

set.seed(123)

# === AR(1) models ===
ar_params <- c(-0.23, 0.81, 0.42, 0.18, -0.62)
for (i in 1:5) {
  x <- arima.sim(n = 200, list(ar = ar_params[i]))
  plot_acf_pacf(x, paste("AR(1) φ =", ar_params[i]))
}

# === MA(1) models ===
ma_params <- c(-0.62, -0.80, 0.66, 0.18, 0.37)
for (i in 1:5) {
  x <- arima.sim(n = 200, list(ma = ma_params[i]))
  plot_acf_pacf(x, paste("MA(1) θ =", ma_params[i]))
}

# === ARMA(1,1) models ===
arma_params <- list(c(-0.86, 0.85), c(0.6, -0.52), c(-0.57, -0.57), c(-0.35, 0.04), c(-0.12, -0.38))
for (i in 1:5) {
  phi <- arma_params[[i]][1]
  theta <- arma_params[[i]][2]
  x <- arima.sim(n = 200, list(ar = phi, ma = theta))
  plot_acf_pacf(x, paste("ARMA(1,1) φ =", phi, ", θ =", theta))
}

# === ARIMA(1,1,1) models ===
arima_params <- list(c(0.2, 1, -0.65), c(-0.37, 1, -0.24), c(-0.08, 1, 0.51), c(-0.54, 1, 0.03), c(0.17, 1, -0.82))
for (i in 1:5) {
  phi <- arima_params[[i]][1]
  d <- arima_params[[i]][2]
  theta <- arima_params[[i]][3]
  x <- arima.sim(n = 200, list(order = c(1, d, 1), ar = phi, ma = theta))
  plot_acf_pacf(x, paste("ARIMA(1,1,1) φ =", phi, ", θ =", theta))
}
