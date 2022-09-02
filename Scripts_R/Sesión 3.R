#################----------------------#################
#               Sesion 3 - Econometria I               #
#################----------------------#################

# Autores: Shelly Gonzales, Camilo Lozada, Santiago Rivera

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Librerias
install.packages("tidyverse")
install.packages("readxl")

library(tidyverse)
library(readxl)
##--------- Datos ---------
## Datos de tipo corte transversal: Un peri?do de tiempo, algun(as) variable(s), algun(os)
##  individuo(s).
setwd("C:/Users/Shelly G/Documents/Monitoria econoemtria/Semana 3/")
corte_transversal = read_excel("WEO_corte transversal.xlsx")
colnames(corte_transversal) ## La columna que contiene el nombre del pa?s est? mal nombrada,

## Renombrando la columna,
colnames(corte_transversal) = c("PAIS", "PIB", "Desempleo", "Deuda Neta")

## ?Que otro problema tiene la base?
## El desempleo est? c?mo caracter y no como n?mero
sapply(corte_transversal, class)
corte_transversal$Desempleo = as.numeric(corte_transversal$Desempleo)
## Datos Vacios
sum(is.na(corte_transversal))
corte_transversal = corte_transversal %>% drop_na()

## Necesito la inflacion y el la inversi?n total de esos pa?ses, pero la tengo en base, ?Que hago?
##----------- JOINS ------------------
join = read_excel("WEO_JOIN.xlsx")
join$Inversion = as.numeric(join$Inversion)
## LEFT-JOIN: Toma como referencia la tabla de la izquierda y sustrae de la tabla de la derecha los elementos
#             que coinciden con ese criterio.
ljoin = left_join(corte_transversal, join, by="PAIS")
# En este caso, se mantienen la cantidad de pa?ses (11) de la base corte transversal y se extraen los datos de
# de inflacion e inversion que coinciden con esos paises en la base join.
  #*******NOTA: Si la variable seg?n la cual se va a hacer el Join (by="") es de un tipo
  #*            distinto en ambas bases, no les va a funcionar.
## RIGHT-JOIN: Toma como referencia la tabla de la derecha y sustrae de la tabla de la izquierda los elemntos
#               que coinciden con ese criterio.
rjoin = right_join(corte_transversal, join, by="PAIS")
#En este caso, se mantienen la cantidad de paises (36) de la base join y se unifican las variables de la tabla
# corte transversal. Note que como Corte Transversal tiene menos paises, los valores para las variables "PIB"
# "Desempleo" y "Deuda" aparacen vacios en la nueva tabla para los pa?ses que estaban en Join pero no en Corte
# transversal.

## INNER-JOIN: Mezcla las tablas teniendo en cuenta SOLO los elementos comunes de ambas tablas.
injoin1 = inner_join(corte_transversal, join, by ="PAIS")
injoin2 = inner_join(join, corte_transversal, by ="PAIS")

## ?Y si no quiero pegar la inversion?
injoin3 = inner_join(corte_transversal, join[,c("PAIS", "Inflacion")])

## Hagan lo mismo pero ahora quiero una base que tengan los pa?ses comunes de ambas tablas
## pero que tenga solo las variables "PIB" e "Inflaci?n"
injoin4 = inner_join(corte_transversal[,c("PAIS", "PIB")], join[,c("PAIS", "Inflacion")])
#La linea anterior se quita, la idea es que los pelados la hagan. 

## FULL-JOIN: Mezcla las tablas teniendo en cuenta TODOS los elementos de ambas tablas.
fjoin = full_join(corte_transversal, join, by="PAIS")

#------ Otros Graficos Estadisticos -----------

## BOX-PLOT
# Box plot de una variable seg?n toda la muestra.
boxplot(injoin4$PIB)
points(mean(injoin4$PIB), col=9, pch = 19)
# Box plot para una variable seg?n alguna categoria
boxplot(injoin2[,c("PIB", "Inflacion", "Desempleo")], col= rainbow(3))
  # Para a?adir la media al grafico
points(c(mean(injoin2$PIB), mean(injoin2$Inflacion), mean(injoin2$Desempleo)), 
       col=9, pch = 19)

## Almacenando los datos del box plot
databoxplot = boxplot(injoin2[,c("PIB", "Inflacion", "Desempleo")], 
                              col= rainbow(3))
databoxplot # Contiene -  Stats: l bigote inferior, el primer cuartil, la mediana, el tercer cuartil y el bigote superior de cada grupo
            #             n: Numero de observaciones              
            #             conf: cada columna representa los extremos inferior y superior del intervalo de confianza de la mediana.
            #             out: Numero total de valores atipicos. 

# DIAGRAMAS DE BARRAS
  # Esto es mejor para variables que son categoricas o binarias. 
# Creamos un data frame
db = data.frame(bebida =c("Cerveza", "Coca Cola", "Agua", "Jugo"), 
                q = c(6, 10, 4, 15))
barplot(height = db$q, names=db$bebida, col=rainbow(4)) 

