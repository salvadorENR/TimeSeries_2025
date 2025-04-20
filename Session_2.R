#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Partmen I
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ( forecast )
pob <-read.csv ("C:/Users/DELL/Documents/TimeSeries_2025/poblacion_SV.csv", header = FALSE )
y <- ts(pob , start =c (1960) , frequency =1);
holt_pob <-holt (y, alpha = 0.3 , beta = 0.1 , h = 5)
summary ( holt_pob )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Part II
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library ( forecast )
pas <-read.csv ("C:/Users/DELL/Documents/TimeSeries_2025/AirPassengers.csv")
z <- ts(pas [[2]] , start =c (1949) , frequency =12) ;
plot (z)
modelo <- decompose (z, type = " additive ")
plot ( modelo )
modelo $ trend
modelo $ seasonal
modelo $ random
reconstruida <- modelo $ trend + modelo $ seasonal + modelo $ random
#graficar ambas series
plot (z, col = " black ", lwd = 2,
      main = " Serie original vs reconstruida ",
      ylab = " Valor ", xlab = " Tiempo ")
# Agregar la segunda serie ( reconstruida )
lines ( reconstruida , col = "red ", lwd = 2, lty = 2)
# Leyenda
#legend(" topleft ", legend = c(" Original ", " Reconstruida "),