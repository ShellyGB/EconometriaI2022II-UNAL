library(readxl)
food <- read_excel(choose.files())


#-----------------------------------------------------------------------------#
#                             Heterocedasticidad
#-----------------------------------------------------------------------------#


#-----------------------------IDENTIFICACI?N----------------------------------#

#Veamos gr?ficamente c?mo luce la heterocedasticidad
plot(food$food_exp~food$income,pch=20)
# Es clara la relaci?n positiva, pero las observaciones tienden a tener m?s 
#dispersi?n a mayores ingresos

# agregar l?nea de regresi?n a gr?fico existente
mod1 = lm(food_exp~income, data=food)
abline(mod1)

#Otros graficos dicientes
par(mfrow=c(2,2))
plot(mod1)
#La relación entre Residuals vs fitted values muestra que hay observaciones
#para las cuales los residuales son mayores, el QQ-plot muestra que no 
#se puede asumir facilmente que es normal.



#Para verlo con mas claridad podemos observar cómo se comportan los residuales
#respecto a la variable explicativa y los valores ajustados

res = residuals(mod1)
food_est = fitted(mod1)
plot(food$income,res, xlab="income", ylab="residuos",pch=20) # Gr?f. 1
plot(food_est,res, xlab="valores estimados", ylab="residuos",pch=20) # Gr?f. 2

#Se ve igual dado que tenemos una sola variable, de lo contrario
#la grafica contra una variable nos indicaría cuál o cuales
#podrían estar generando problema, la gráfica contra los fitted
#values nos mostraría la presencia de heterocdasticidad


#-----------------------------Pruebas estad?sticas-----------------------------#

# las pruebas habituales son Breush-Pagan y el test de White (H0: Homocedasticidad)
# Goldfeld-Quandt suele emplearse cuando hay explicativas categ?ricas (H0: Homocedasticidad)
library(zoo)
library(lmtest)

# Breush-Pagan: 
#consiste en estimar los residuos al cuadrado del modelo original
# en funci?n de las variables explicativas originales (H0: Homocedasticidad)
bptest(mod1)
bptest(formula = food_exp~income,data = food)#Estas dos lineas son equivalentes
#Resultado: El p-valor es menor a 1, por lo tanto se rechaza la H0 y se concluye
#que hay evidencia estadística para creer que los residuales son heterocedasticos


# White: 
#es la prueba B-P pero modifica la f?rmula, agregando formas cuadr?ticas
# o interacciones de las variables explicativas (H0: Homocedasticidad)
bptest(formula = food_exp~income+I(income^2),data = food)
#Resultado: El p-valor es menor a 1, por lo tanto se rechaza la H0 y se concluye
#que hay evidencia estadística para creer que los residuales son heterocedasticos



#Goldfedl-Quandt (H0: Homocedasticidad)
#Consiste en estimar la varianza para dos grupos de la muestra y ver
#si estas varianzas son estadísticamente diferentes.
gqtest(mod1, point=0.4, order.by=~income, alternative = "greater", data=food)
#Resultado: El p-valor es menor a 1, por lo tanto se rechaza la H0 y se concluye
#que hay evidencia estadística para creer que los residuales son heterocedasticos


#Oder.by ordena según una variable, la que crea que genera el cambio
#en la varianza, y point indica le punto (proporción) en el que creo que
#se genera el rompimierto, por eso suele usarse con variables categoricas
#alternative indica cual es la alternativa, si que crezca con x (greater), que 
#decrezca con x (less) o que sea two-side



#----------------------------Correción---------------------------------#
#Robustos
library(carData)
library(car)
# Matriz de var-cov de los coeficientes bajo heterocedasticidad.
vcov(mod1) # esta es la original

#Robustos: se emplea la funci?n hccm(): heteroscedasticity-corrected covariance matrices
hccm(mod1) # esta es la corregida 
hccm(mod1,type = "hc0") #Sin ajustar por grados de libertad
hccm(mod1,type = "hc1") #Ajustando por grados de libertad

# Comparemos OLS vs errores est?ndar robustos
coeftest(mod1)
mod1.HC1 = coeftest(mod1,vcov. = hccm(mod1,type = "hc1"));mod1.HC1
# los errores est?ndar corregidos son menores, permitiendo ganar singificancia 
# pues el estad?stico t es mayor y el p-valor menor para ambos coeficientes


#Mínimos cuadrados generalizados y MCG Factibles

#Generalizados
#Para cuando conozco la forma de la varianza, aquí asumiremos que es creciente
#Si var(e) = sigma1*income, entonces el factor de correción será:
w = 1/food$income 
#Estimando:
mod1.wls = lm(food_exp~income, weights=w, data=food)

#Factibles

# FGLS funciona de manera similar, en este m?todo no es requisito asumir forma de la varianza
# las variables se transforman empleando como ponderaci?n el residuo cuadrado
vari <- resid(mod1)^2
mod.fgls <- lm(food_exp~income, weights=1/vari, data=food)

stargazer(mod1,mod1.HC1,mod1.wls,mod.fgls,type = "text")


#-----------------------------------------------------------------------------#
#                             Autocorrelación
#----------------------------------------------------------------------------#
library(readr)
interes <- read_csv(choose.files())
modelo_lineal = lm(TINTER ~ SALDOPP, data=interes)

#Ver si existe alguna relación entre los residuales y su primer rezago
plot(modelo_lineal$residuals[2:77], modelo_lineal$residuals[1:77-1])

#Durbin-watson test 
#Consiste en evaluar la hipotesis de que no hay correlación serial entre
#los errores contra que los errores sigan una correlación de orden 1 
#(H0: no autocorrelation) - Solo para correlación de primer orden
dwtest(modelo_lineal,alternative = "two.sided",iterations = 1000)

#Alternative es para evaluar que la correlación sea de con un coeficiente
#menor a 0, mayor a 0 o diferente de cero, para propositos de este curso
#lo improtante es que sea sidferente de cero. Las iteraciones son para
#calcular el p-valor.


#Breusch-godfrey (multiplicadores de lagrange)
#Sigue la misma logica que el durbin watson pero se puede evaluar
#para una correlación de cualquier orden
bgtest(modelo_lineal,order = 5)


#---------------------------Correción---------------------------------------#
library(sandwich)

#Minimos cuadrados generalizados 
#Vamos a suponer que la correlación es de orden 1, de forma que 
#el nuevo modelo es Y= Yt - pYt-1, donde p es la correlación entre
#el error y su primer rezago, hay que estimarlo

#p
ModRes = lm(modelo_lineal$residuals[1:76] ~ modelo_lineal$residuals[2:77])

#Variables nuevas
NewY =  interes$TINTER[1:nrow(interes)-1] - ModRes$coefficients[1]*interes$TINTER[2:nrow(interes)]
NewX = interes$SALDOPP[1:nrow(interes)-1] - ModRes$coefficients[1]*interes$SALDOPP[2:nrow(interes)]

#Regresión
NewReg = lm(NewY~NewX)

vcov(modelo_lineal)
vcov(NewReg)  

#Robustos a autocorrelacion o HAC standard errors
vcovHAC(modelo_lineal)
NeweyWest(modelo_lineal)
