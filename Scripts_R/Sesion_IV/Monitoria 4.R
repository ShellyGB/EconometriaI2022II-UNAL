
# ------------------------------- Script ----------------------------------
#                     Universidad Nacional de Colombia
#                     Facultad de Ciencias Económicas
#                              Econometr??a I
# ------------------------------ Monitoria --------------------------------

remove(list = ls()) #Limpiar entorno

#Modelo Lineal Simple:
# Y=Xβ+U
# se puede expresar como
# Yi=β0+β1Xi+Ui
  

  y <- c(2, 3, 5, 6, 8, 9, 10) #creando un vector con 10 elementos
  y
  x <- c(1, 2, 3, 4, 5, 6, 7) #creando un vector con 10 elementos
  x
#Relice el gráfico de dispersión
  plot(x,y)
  
  
#Encuentre el modelo estimado
#Primero Creamos la matriz de datos X
x<-cbind((1:1),x);x #aumentamos una columna de unos que hacen referencia a 
  
#Encontrando el vector β de coeficientes
  
t(x)#hallando la transpuesta
 
(t(x)%*%x)#hallando la matrix XTX, como es producto de matrices usamos %*%

solve(t(x)%*%x)#calculando la inversa de XTX

t(x)%*%y#hallando XTY como x es una matriz se usa el producto %*%

b<-(solve(t(x)%*%x))%*%(t(x)%*%y)#aplicando la formula de b por mco
b



#β=(0.57 1.39)
  
#Asi entonces el modelo estimado es:
#   Yˆi=0.57+1.39Xi
  
#Valores de Yˆi
#y estimado
  yest<-x%*%b
#determinando y estimado con el vector de parametros estimados b
  
#Error del modelo
  e<-y-yest
  e
  
#Interprete los estimadores
#Encontrando el coeficiente de determinación R2
  
#SCE = STC – SCR
#SCE = Suma de cuadrados residual
#STC = Suma total de cuadrados
#SCR = Suma de cuadrados de la regresión

sce1<- yest - mean(y) 
sce = t(sce1)%*%sce1
sce<-(t(yest)%*%yest)-7*(mean(y))^2
sct<-(t(y)%*%y)-7*(mean(y))^2

#R2
R2<-sce/sct
  
data <- as.data.frame(cbind((1:1),x))
regresion <- lm(y ~ x, data = data)
summary(regresion)
