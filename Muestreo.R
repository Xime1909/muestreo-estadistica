
attach(Muestreo_Estadisctica)
Numerodepuntosnegros

Muestreo_Estadisctica<-data.frame(Muestreo_Estadisctica)
dim(Muestreo_Estadisctica)

summary(Muestreo_Estadisctica) #Resumen de los datos
Muestreo_Estadisctica<-basicStats(Numerodepuntosnegros)
descriptivos <- summary(Muestreo_Estadisctica)

tablafq<-table(Numerodepuntosnegros)#Frecuencias absolutas
tablafq

stem(Numerodepuntosnegros)
tablaPuntos<- as.data.frame(table(Numerodepuntosnegros))
tablaPuntos

boxplot(Numerodepuntosnegros) #Cajas y bigotes
boxplot(scale(Numerodepuntosnegros), horizontal =TRUE)
boxplot

hist(Muestreo_Estadisctica)
hist(Muestreo_Estadisctica, main = "Histograma de frecuencias Numero de puntos negros",
     xlab = "Numero de puntos negros",
     ylab = "Numero de baldosa",
     col = "black",
     border = "blue",
     xlim = c(0, 106),
     ylim = c(0,100))
var(Numerodepuntosnegros,na.rm = TRUE) #Varianza
sd(Numerodepuntosnegros,na.rm = TRUE) #Desviacion estandar
sd(Numerodepuntosnegros)/mean(Numerodepuntosnegros) #Coeficiente de variacion

skew(Numerodepuntosnegros) #coeficientes de simetria
kurtosi(Numerodepuntosnegros) #curtosis

skew(Numerodepuntosnegros)/sqrt(6/20) #Estandarizada
kurtosi(Numerodepuntosnegros)/sqrt(6/20) #Estandarizada

f <- table(Numerodepuntosnegros)
f_porc <- round((Numerodepuntosnegros*100),2)
f_porc_acum <- round(cumsum(prop.table(tabla)*100),2)

prop.table(Numerodepuntosnegros) #Frecuencias relativas
round((prop.table(Numerodepuntosnegros)*100),2)

write.csv2(f, file = "Tabla 1.csv")
write.csv2(f_porc, file= "Tabla 2.csv")
write.csv2(f_porc_acum, file= "Tabla 3.csv")

cumsum(Numerodepuntosnegros) #Frecuencias absolutas acumuladas
quantile(Numerodepuntosnegros, prob = c(0.25, 0.5, 0.75), na.rm = TRUE)# Medidas de posicion

