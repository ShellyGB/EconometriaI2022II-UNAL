## -------------------------------------------------------- ##
#                 Sesion 6 - Econometría I                   #
## -------------------------------------------------------- ##
## Script por: Shelly Gónzalez, Camilo Lozada, Santiago Rivera 

## Estimación por el método de máxima verosimilitud. 

# Concepto:
#           Verosimilitud - Probabilidad
#       Maxima Verosimilitud - Máxima Probabilidad. 

## Se trata de estimar los parámetros de la distribución correspondiente, 
## que maximizan la probabilidad de encontrar los valores de la distribución poblacional.

## Veamoslo, 
set.seed(1123) # Plantamos una semilla, 
x = rnorm(100) # Creamos una variable x que sigue una distribución normal 
## Escalamos para que la desviacion estandar(x)=8 y la media sea 10. 
x = x/sd(x)*8 # Fijando la Desviación extándar
x = x-mean(x)+10 # Fijando la media
## Ahora la variable x sigue una distribución normal con media 10 y varianza 100. 
## Veamos la distribución de la variable con un histograma.
hist(x)
# Recordemos que la función de densidad de probabilidad para la distribución normal es: 
fdp_norm = function(x,m,s){y = (1/sqrt(2*pi*s^2))*exp((x-m)^2/(-1/(2*s^2)))}
## Veamos lo gráficamente, 
# Distribición normal estándar, 
curve(expr = fdp_norm(x = x, m=0, s=1), from = -4, to = 4)
# Distribición normal con media = 1 y ds = 1 
curve(expr = fdp_norm(x = x, m=1, s=1), from = -4, to = 4, add = T, col="red")
# Distribución normal con media = 0 y ds = 2
curve(expr = fdp_norm(x = x, m=0, s=2), from = -4, to = 4, add = T, col="green")

# ¿Cuál distribución entre Y y Z puede explicar de una mejor manera mi variable de interés?
# El método de máxima verosimilitud lo que hace es hallar los mejores parámetros de la distribución,
# que se ajustan a la función de densidad de probabilidad de mi variable. 

## Formalmente: Se tiene una función de densidad conjunta dónde se tratan de encontrar un conjunto de datos,
##              dados unos parámetros. 
## Sin embargo, se trata de responder lo contrario: 
##      ¿Qué tan probable es hallar una muestra dados unos parámetros?
## Lo anterior dado a que en lo práctico se tienen como observado la muestra de datos y no los parámetros.
## LA IDEA ENTONCES ES - HALLAR EL VECTOR DE PARÁMETROS QUE MÁXIMIZA ESA FUNCIÓN. 

##----- Máxima verosimilitud en el modelo de regresión lineal simple. 
# Siguiendo lo anterior, en una regresión   Y~X lo que se trata es hallar los parámetros que máximizan 
# la probabilidad de encontrar la Y dada la serie X.
# ¿Qué parámetros? Depende de la distribución. 
## En el caso de la regresión lineal simple, el supone que el término de error se distribuye normalmente. 

## Estimemos una regresion lineal por maxima verosimilitud. 
## Datos
x= cbind(1, runif(100)) #Generamos las variables X para correr la regresion, 
param_poblacional = c(2,3,1) # Definimos los parametros poblacionales: b0=2, b1=3 y varianza=1
y = x %*%param_poblacional[1:2] + rnorm(100)

## Funcion de verosimilitud,
## Funcion de verosimilitud
lm_MLE = function(theta, y, x) {
  n = nrow(x) 
  k = ncol(x) 
  beta = theta[1:k] 
  sigma2 = theta[k+1] 
  e = y-x%*%beta 
  logl = -((n/2)*log(2*pi))-((n/2)*log(sigma2))-(t(e)%*%e/(2*sigma2))
  return(-logl)
  }

 


max = optim(c(1,1,1), lm_MLE, method = "BFGS", hessian = T, y=y, x=x)
max

## Matriz de informacion y varianzas asintoticas,
max$hessian
OI = solve(max$hessian)
SE = sqrt(diag(OI)) ## Desviaciones estandar

## Veamos si da lo mismo por mco,
datos = data.frame(x=x[,2], y)
lm_mco = lm(y~x, data=datos)
summary(lm_mco)
## Comparamos
tablita = cbind("Coeficientes MLE"= c(max$par[1:2]), "Desviaciones MLE" = SE[1:2],
                "Coeficientes MCO"= lm_mco$coefficients, "Desviaciones MCO" = sqrt(diag(vcov(lm_mco)))) 

## ¿Por qué dan iguales?
