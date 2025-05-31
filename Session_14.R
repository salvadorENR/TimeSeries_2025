# Lista de paquetes requeridos
required_packages <- c("forecast", "tseries", "urca", "lmtest")

# Instalar paquetes que no estén ya instalados
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Cargar todas las librerías
invisible(lapply(required_packages, library, character.only = TRUE))


# Aplicar el test de Dickey-Fuller aumentado para verificar si la serie z es estacionaria (prueba de raíz unitaria)
adf.test(z)

# Aplicar el test de Phillips-Perron, una alternativa robusta al ADF para verificar la estacionariedad de z
pp.test(z)

# Aplicar el test KPSS, que prueba la hipótesis nula de que la serie es estacionaria
kpss.test(z)

# Ajustar automáticamente un modelo ARIMA a la serie z usando el criterio AIC/BIC y pruebas de diagnóstico
fit <- auto.arima(z)

# Realizar el test de Ljung-Box sobre los residuos del modelo ajustado para verificar si hay autocorrelación remanente
Box.test(residuals(fit), lag = 20, type = "Ljung-Box")

# Aplicar el test de Shapiro-Wilk para verificar la normalidad de los residuos del modelo ajustado
shapiro.test(residuals(fit))

# Ajustar manualmente un modelo ARIMA(p,d,q) a la serie z (los valores de p, d y q deben definirse previamente)
fit <- arima(z, order = c(p, d, q))

# Calcular el logaritmo de la verosimilitud del modelo ajustado
logLik(fit)

# Calcular el AIC (Criterio de Información de Akaike) para comparar modelos
AIC(fit)

# Calcular el BIC (Criterio de Información Bayesiano) para comparar modelos
BIC(fit)
