## -------------------------------------------------------- ##
#                 Sesion 9 - Econometr�a I                   #
## -------------------------------------------------------- ##
## Script por: Shelly G�nzalez, Camilo Lozada, Santiago Rivera

## M�todo de simulaci�n de Monte Carlo.

## Simulaci�n:  Mediante la repetici�n de un experimento aleatorio
# una cantidad de veces, se pretende llegar a la reproducci�n
# de la realidad. Ejemplo: Lanzar una moneda X veces, para hallar
#                          la probabilidad de que salga cara.
# Lo anterior se hace mediante la generaci�n de n�meros aleatorios.

## En econometr�a el m�todo de simulaci�n de Monte Carlo, se puede usar
# para verificar las propiedades de los estimadores MCO.

## Verifiquemos que los estimadores MCO son insezgados.

## Para ello, creamos una serie de muestras aleatorias con T=10 observaciones
# que pretende explicar el modelo estad�stico:
#                       y = B + e_i
# Los par�metros poblacionales del modelo son, arbitrariamente:
# B = 20, sigma^2 = 10
# Por lo tanto, e~N(0,10) y y_i~(20,10).

# limpiar todo
remove(list = ls())

## Definiendo los par�metros:
T = 10 ## N�mero total de observaciones por muestra
B = 20 ## Par�metro poblacional
sigma2 = 10 ## Varianza

set.seed(12345) # Plantamos una semilla

## Por lo tanto, y = 20 + e_t~N(0,20), esto es

y = B + rnorm(T, mean = 0, sd= sqrt(sigma2))
y
#%------------------------------------------------------------------------%#
## La funci�n rnorm() me permite generar una serie de T n�mero aleatorios ##
## que siguen una distribuci�n normal con una media y una varianza dada.  ##
#%------------------------------------------------------------------------%#

## El estimador por MCO para este modelo con dichos datos es:
b = sum(y)/T

## Ahora hagamos ese proceso mil veces, (Simulaci�n de Monte Carlo)
matriz_y = matrix(ncol=1000,nrow=T) # Definimos una matriz con mil columnas y
# 10 filas para guardar las 1000 muestras.
for (i in 1:1000) {
  e= rnorm(T, mean = 0, sd= sqrt(sigma2))
  y = B + e
  matriz_y[,i] = y #Guardamos los 10 datos y de las 1000 simulaciones por columnas.
}
matriz_b = matrix(ncol=1000, nrow=1) # Creamos la matriz donde vamos a guardar
# los betas

## Calculamos los 1000 betas:
for  (i in 1:1000) {
  matriz_b[1,i] = sum(matriz_y[,i])/T ## Guardamos los betas por columnas
}

## Una vez se tienen los 1000 betas, calculamos el promedio de estos.
b_MC = sum(matriz_b[1,])/1000

## Esto signfica que el beta estimado por MCO en 1000 muestras aleatorias, es
# en promedio 19.9978 que es muy cercano al par�metro poblacional B=20.

## Ahora hallemos el sigma cuadrado estimado del beta,
sigma_mc = sum((matriz_b[1,] - b_MC)^2)/999
## La varianza de los betas sigma^2/T estimada es 0.9875 que es muy cercano
# al par�metro poblacional = 1.

## Las simulaciones de Monte Carlo, permiten tambi�n analizar emp�ricamente
## problema de sobre-especificaci�n u omisi�n de variables de un modelo.

## Supongamos que se tiene un modelo correctamente especificado que sigue la forma:
#          q_t= b_0 + b_1 x_1t + b_2 x_2t + beta_3 x_3t + e_t
# los valores poblacionales son:
# b_0 = 15, b_1 = -1.6, b_2 = 0.7, b_3 = 0, sigma^2 = 21.

# Es decir, el modelo correctamente especificado sigue la forma:
#         q_t= -9.5 + 13 p_t + 0.7 y_t + 0 ps_t + e_t

remove(list = ls()) #Limpiamos el enviroment.

## Subamos los datos,
install.packages("readxl")
library(readxl)

tabla_MC = read_xlsx(file.choose()) # Elegimos el archivo de la carpeta d�nde est�
attach(tabla_MC)
bienx = as.matrix(tabla_MC[,-4]) ## Quitando la cuarta columna dado que sabemos que
                                 # b_3 = 0
## Ahora establezcamos los par�metros del modelo correctamente especificado.
simular = 8000 # Vamos a hacer 8000 iteraciones.
b0 = 15
b1 = -1.6
b2 = 0.7
b3 = 0
sigma2 = 16
T = nrow(tabla_MC) #N�mero de observaciones.

## Matriz de varianzas y covarianzas te�rica o poblacional.
var_cov = sigma2*solve(t(bienx)%*%bienx)
colnames(var_cov) = c("b_0", "b_1", "b_2") 
var_betas_teoricos = diag(var_cov)
set.seed(41252)
## Simulamos la variable q_t poblacional,
matriz_q = matrix(nrow = T,ncol = simular) # Voy a tener 21 q_t por cada simulaci�n.
for (i in 1:simular) {
  e = rnorm(n=21,mean=0,sd=sqrt(sigma2))  # Estimamos errores de manera aleatoria, que siguen
                                          # una distribuci�n e_t ~ N(0,16)
  q = b0 + b1*p + b2*y + e
  matriz_q[,i] = q
}

## Una vez se tienen 8000 mil muestras de 21 observaciones de la variable q_t, procedemos
# a estimar el modelo que est� correctamente especificado, el modelo que omite variables y
# el modelo sobre identificado.

## Modelo Correctamente especificado: q_t= b_0 + b_1 p_t + b_2 y_t + e_t
beta_correcto = matrix(nrow = simular, ncol = 3)  # Matriz d�nde vamos a guardar los 8000 par�metros
                                                  # estimados por cada beta
colnames(beta_correcto) = c("b_0", "b_1", "b_2")
for (i in 1:simular) {                  
  reg = lm(matriz_q[,i] ~ p + y)  
  beta_correcto[i,] = reg$coefficients
}
## Calculamos los betas promedio de las 8000 iteraciones y sus varianzas.
Parametros_correct = c(round(colMeans(beta_correcto),4), "b_3"=0) # Betas estimados.
library(resample)
sigma2_correct = c(round(colVars(beta_correcto),4),"b_3"=0) # Varianza de los betas.
## los par�metros son aproximadamente los poblaciones. 

## Modelo que omite variables importantes: q_t= b_0 + b_1 p_t + e_t
beta_omit = matrix(nrow = simular, ncol = 2)  
colnames(beta_omit) = c("b_0", "b_1")
for (i in 1:simular) {                  
  reg = lm(matriz_q[,i] ~ p)  
  beta_omit[i,] = reg$coefficients
}
Parametros_omit = c(round(colMeans(beta_omit),4), "b_2" = 0, "b_3"=0) # Betas estimados.
sigma2_omit = c(round(colVars(beta_omit,4)), "b_2" = 0, "b_3"=0) # Varianza de los betas.

## Modelo sobre-especificado: q_t= b_0 + b_1 p_t + b_2 y_t + b_3 ps + e_t
beta_sobre = matrix(nrow = simular, ncol = 4)  
colnames(beta_sobre) = c("b_0", "b_1", "b_2", "b_3")
for (i in 1:simular) {                  
  reg = lm(matriz_q[,i] ~ p + y + ps)  
  beta_sobre[i,] = reg$coefficients
}
Parametros_sobre = c(round(colMeans(beta_sobre),4)) # Betas estimados.
sigma2_sobre = c(round(colVars(beta_sobre,4))) # Varianza de los betas.

## Tabla Comparativa
# Betas
Tablita = rbind("Betas Poblacionales" = c(b0, b1, b2, b3), "Sigma2 Poblaciones"= c(var_betas_teoricos, "b_3"=0)
                ,"Especificaci�n Correcta" = Parametros_correct, "Sigma2 Correcta" =sigma2_correct, 
                "Especifici�n Omite" = Parametros_omit, "Sigma2 Omite" = sigma2_omit,
                "Especificaci�n Sopre-especifica" = Parametros_sobre, "Sigma2 Sobre-especifica" = sigma2_sobre)
colnames(Tablita) = c("b_0", "b_1", "b_2", "b_3")
Tablita = round(Tablita, 2)
Tablita

## Intepreten los resultados. 
