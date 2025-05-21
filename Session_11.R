#Creating the data set
x=c(-1.043,1.684,1.063,-3.109,-0.038,-0.752,-1.025,-2.016,-4.626,-2.592)
#Adjusting the model
modelo_estimado=arima(x,order=c(1,0,0),method = "ML")
summary(modelo_estimado)
