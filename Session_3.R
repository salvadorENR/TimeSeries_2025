#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#       Modelo con funciones periódicas
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#******************************************************************************
#* Base de datos nottem
#******************************************************************************

#1.Cargar la serie
data(nottem)
ts_data<-nottem
plot(ts_data)

#2.Preparar tiempo y frecuencia
n<-length(ts_data)
t<-1:n
omega<-2*pi/12#frecuencia estacional (mensual, 12 periodos)

#3.Crear variables armonicas y ajustar modelo
harmonicos<-data.frame(
  y=as.numeric(ts_data),
  sin=sin(omega*t),
  cos=cos(omega*t)
)
modelo<-lm(y~sin+cos,data=harmonicos)

#4.Ajuste como serie temporal
ajuste<-ts(fitted(modelo), start=start(ts_data),frequency=frequency(ts_data))

#5.Graficar resultado
plot(ts_data, type="l", col="black",lwd=2, main="Modelo Armonico ajustado a 'nottem'",ylab="Temperatura (F)",xlab="Tiempo")
lines(ajuste, col="blue", lwd=2)
legend("topright", legend=c("Datos reales","Modelo aremonico"),col=c("black","blue"),lty=1)

#6.Mostrar resumen del modelo
summary(modelo)

#******************************************************************************
#        Base de datos UKgas     
#******************************************************************************

#1.Cargar la serie
data("UKgas")
ts_data<-UKgas
plot(ts_data)

#2.Preparar tiempo y frecuencia
n<-length(ts_data)
t<-1:n
omega<-2*pi/12#frecuencia estacional (mensual, 12 periodos)

#3.Crear variables armonicas y ajustar modelo
harmonicos<-data.frame(
  y=as.numeric(ts_data),
  sin=sin(omega*t),
  cos=cos(omega*t)
)
modelo<-lm(y~sin+cos,data=harmonicos)

#4.Ajuste como serie temporal
ajuste<-ts(fitted(modelo), start=start(ts_data),frequency=frequency(ts_data)
)

#5.Graficar resultado
plot(ts_data, type="l", col="black",lwd=2, main="Modelo Armonico ajustado a 'nottem'",ylab="Temperatura (F)",xlab="Tiempo")
lines(ajuste, col="blue", lwd=2)
legend("topright", legend=c("Datos reales","Modelo aremonico"),col=c("black","blue"),lty=1)

#6.Mostrar resumen del modelo
summary(modelo)

#******************************************************************************
#+ Base de datos co2
#******************************************************************************

#1.Cargar la serie
data("co2")
ts_data<-co2
plot(co2)

#2.Preparar tiempo y frecuencia
n<-length(ts_data)
t<-1:n
omega<-2*pi/12#frecuencia estacional (mensual, 12 periodos)

#3.Crear variables armonicas y ajustar modelo
harmonicos<-data.frame(
  y=as.numeric(ts_data),
  sin=sin(omega*t),
  cos=cos(omega*t)
)
modelo<-lm(y~sin+cos,data=harmonicos)

#4.Ajuste como serie temporal
ajuste<-ts(fitted(modelo), start=start(ts_data),frequency=frequency(ts_data))

#5.Graficar resultado
plot(ts_data, type="l", col="black",lwd=2, main="Modelo Armonico ajustado a 'nottem'",ylab="Temperatura (F)",xlab="Tiempo")
lines(ajuste, col="blue", lwd=2)
legend("topright", legend=c("Datos reales","Modelo aremonico"),col=c("black","blue"),lty=1)

#6.Mostrar resumen del modelo
summary(modelo)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+  Periodograma
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1. Cargar datos
data("AirPassengers")
ts_data <- AirPassengers

# 2. Calcular el periodograma
spec <- spectrum(ts_data , log = "no", plot = TRUE)

# 3. Obtener las frecuencias dominantes
frecs <- spec$freq
potencia <- spec$spec
orden <- order(potencia , decreasing = TRUE)
top_k <- 3
frecs_top <- frecs[orden [1: top_k]]

# 4. Crear variables a r m n i c a s (sin y cos)
t <- 1: length(ts_data)
harmonicos <- data.frame(t = t)
for (j in 1:top_k) {
  omega <- 2 * pi * frecs_top[j]
  harmonicos [[ paste0("sin", j)]] <- sin(omega * t)
  harmonicos [[ paste0("cos", j)]] <- cos(omega * t)
}

# 5. Ajustar el modelo
modelo <- lm(ts_data ~ ., data = harmonicos)


# 6. Obtener el ajuste como serie ts
ajuste <- ts(fitted(modelo), start = start(ts_data),
             frequency = frequency(ts_data))

# 7. Graficar resultado
plot(ts_data , type = "l", col = "black", lwd = 2, main = "
Modelo A r m n i c o Ajustado", ylab = "Pasajeros")
lines(ajuste , col = "blue", lwd = 2)
legend("topleft", legend = c("Datos reales", "Modelo
a r m n i c o "), col = c("black", "blue"), lty = 1)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Transformación de la serie
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1. Datos y log - transformaci n
data("AirPassengers")
ts_data <- log(AirPassengers)
# 2. Calcular el periodograma
spec <- spectrum(ts_data , log = "no", plot = TRUE)
# 3. Obtener las frecuencias dominantes
frecs <- spec$freq
potencia <- spec$spec
orden <- order(potencia , decreasing = TRUE)
top_k <- 3
frecs_top <- frecs[orden [1: top_k]]
# 4. Crear variables a r m n i c a s (sin y cos)
t <- 1: length(ts_data)
harmonicos <- data.frame(t = t)
for (j in 1:top_k) {
  omega <- 2 * pi * frecs_top[j]
  harmonicos [[ paste0("sin", j)]] <- sin(omega * t)
  harmonicos [[ paste0("cos", j)]] <- cos(omega * t)
}
# 5. Ajustar el modelo
modelo <- lm(ts_data ~ ., data = harmonicos)
# 6. Obtener el ajuste como serie ts
ajuste <- ts(fitted(modelo), start = start(ts_data),
             frequency = frequency(ts_data))
# 7. Graficar resultado
plot(ts_data , type = "l", col = "black", lwd = 2, main = "
Modelo A r m n i c o Ajustado", ylab = "log(Pasajeros)")
lines(ajuste , col = "blue", lwd = 2)
legend("topleft", legend = c("Datos reales", "Modelo
a r m n i c o "), col = c("black", "blue"), lty = 1)




