install.packages("tseries")
library(tseries)
# Cargar serie temporal integrada en R
data ("Nile" )
#Visualizar la serie temporal
plot(Nile,main="Serie temporal: Caudal anual del río Nilo", ylab="Caudal", xlab=" A o") 
#Calcular estimadores básicos
media <- mean(Nile)
varianza<-var(Nile)
#Calcular auto correlaciones
acf_vals<- acf(Nile, main= "Correlograma de la serie Nile"
                            , plot=TRUE)

#Prueba de Dickey-Fuller aumentada
adf_result<-adf.test(Nile)
#Mostrar resultados
cat("Media:", media,"\n")
cat("Varianza:", varianza, "\n") 
print(adf_result)
# Calcular autocorrelaciones
acf_vals<-acf(Nile,main="Correlograma de la serie Nile"
                            ,plot=TRUE)

#Prueba de Dickey-Fuller  aumentada
adf_result<-adf.test(Nile)
#Mostrar resultados
cat("Media: ", media, "\n" )
cat ("Varianza:",varianza, "\n") 
print(adf_result)


#Cargar la serie 
data("Nile")
#Diferencia de primer orden 
Nile_diff<-diff(Nile)
#Graficar la serie diferenciada
plot(Nile_diff, main="Primera diferencia de la serie Nile"
          , ylab= "Cambio en caudal" , xlab=" A o ")
#Correlograma(ACF)
acf(Nile_diff, main= "Correlograma de la serie diferenciada
(Nile_diff)")
#Prueba de Dickey-Fuller para verificar estacionariedad 
adf.test(Nile_diff)