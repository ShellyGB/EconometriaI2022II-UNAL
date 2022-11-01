# ------------------------------- Script ----------------------------------
#                     Universidad Nacional de Colombia
#                     Facultad de Ciencias Económicas
#                              Econometr??a I
# ------------------------------ Monitoria --------------------------------

rm(list = ls()) # Limpiamos el entorno

#descargamos el archivo csv desde el github de la clase
Data <- read.csv('https://raw.githubusercontent.com/ShellyGB/EconometriaI2022II-UNAL/main/Scripts_R/Sesion_X/tabla_costos.csv', sep = ';')

#Estimaremos un modelo con la siguiente ecuación de costos

#yi= B1 + B2xi + B3(xi)^2 + e

#Revisemos nuestros datos 

class(Data[,1])
class(Data$ct)
class(Data$q)

#q y ct son tipo character porque tiene comas en vez de puntos, 
#por tanto hay que reemplazar las comas por puntos y convertirlos a nuemric

#gsub() nos ayuda a reemplazar las comas por puntos en un vector
Data$ct <- gsub(",",".",Data$ct)
Data$ct <- as.numeric(Data$ct)

Data$q <- gsub(",",".",Data$q)
Data$q <- as.numeric(Data$q)

#Ahora que nuestras variables son numéricas, podemos hacer nuestra regresión con normalidad

reg_costos <- 
  lm(ct ~ q + I(q^2) , Data)

summary(reg_costos)

#Note que ambos coeficientes son  estadísticamente 
#significativos a un 5% de significancia

# Ahora analicemos las funciones de CMe, CT y CMg

#CT <- B1 + B2*x + B3*(x^2)
#(suma de todos los costos)
#CMg <- B2 + 2*B3*x 
#Derivada parcial con respecto a x
#CMe <- B1/x + B2 + B3*x
#(yt/xt)

#Para estimar estas ecuaciones guardaremos los coeficientes.

B1 <- reg_costos$coefficients[1]
B2 <- reg_costos$coefficients[2]
B3 <- reg_costos$coefficients[3]

#Estimaremos los costos de x=(0:40), incrementando en 0.01
x <- seq(0, 40, by = 0.01)

CT <- B1 + B2*x + B3*(x^2)
CMg <- B2 + 2*B3*x 
CMe <- B1/x + B2 + B3*x

#Ahora graficaremos para comprobar el coomportamiento 
#de nuestras funciones de costo

x11()

plot(x, CT, 
     type = "l", col = "blue", 
     main = "Funciones de costo", 
     ylab = "Costo",
     xlab = "Q")
lines(x, CMe, col = "red")
lines(x, CMg, col = "darkgreen")
legend(25, 9500, 
       legend = c("Coste total", "Coste medio", "Coste marginal"),
       col = c("blue", "red", "green"), 
       lty = 1:1, cex = 0.55)

#En equilibrio bajo competencia perfecta debemos tener que

#CMe=CMg=P

#Donde el precio de mercado es igual al Costo medio
#Al igualar la funcion de Costo medio y Costo Mg obtenemos

#B3x^2-B1=0

a=B3
b=0
c=-B1

#Discriminante
delta<-function(a,b,c){
  b^2-4*a*c
}

x_1 = ((-b+sqrt(delta(a,b,c)))/(2*a))
x_2 = ((-b-sqrt(delta(a,b,c)))/(2*a))

#Tomamos x_1 porque es positivo, que será nuestra q en equilibrio

P <- B2 + 2*B3*x_1 

#Por tanto,

#El precio y cantidad en equilibrio son

#P=56.80001
#Cantidad=6.477753

#Ejercicio 2 - series de tiempo

#estimemos el consumo mediente el producto y sus rezagos

library(dplyr)
library(PerformanceAnalytics)

Data2 <- read.csv('https://raw.githubusercontent.com/ShellyGB/EconometriaI2022II-UNAL/main/Scripts_R/Sesion_X/consumo_producto.csv', sep = ';')

class(Data2$year)

#Necesitamos que year sea tipo Date

Data2 <- Data2 %>%
  mutate(year = as.Date(ISOdate(year, 1, 1)))

class(Data2$year)

#Ahora creamos 2 vectores tipo xts con la ayuda de 
#el paquete PerformanceAnalytics

yts <- xts(Data2$y, order.by=Data2$year)
cts <- xts(Data2$c, order.by=Data2$year)

#Grafiquemos nuestras series de tiempo

chart.TimeSeries(yts, colorset = 'darkgreen', 
                 main="PIB")

chart.TimeSeries(cts, colorset = 'darkblue', 
                 main="PIB")

# Se crea la variable de rezago:
Data2$Rezago <- c(0, Data2$c[1:nrow(Data2)-1])

# Como se "perdió" una observación, eliminamos la primera fila:
Data2 <- Data2[2:31, ]

# Estimamos normalmente:
reg2 <- lm(c ~ y + Rezago, data = Data2)
summary(reg2)
