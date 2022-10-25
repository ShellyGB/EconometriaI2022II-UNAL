library(haven)
library(tidyverse)
library(patchwork)
library(vcov)
MincerianEconometriaI <- read_dta("Practica Banrep/Informality and missalocation DS/MincerianEconometriaI.dta")
colnames(MincerianEconometriaI)
colnames(MincerianEconometriaI)[9] = "Ingreso"

#Remove na
MincerianEconometriaI = MincerianEconometriaI %>% drop_na()

#----------------------Regresion minceriana-------------------------------#
#Mincer identificó que los retornos a la experiencia tienen forma cuadratica,
#una regresión minceriana se considera aquella que explica el ingreso en
#en terminos de la educación y el ingreso, aquí añadiremos el genero

#Creas variable de experiencia: Edad - 15
MincerianEconometriaI = MincerianEconometriaI %>% mutate(Exper = if_else(edad<=15, 0, edad-15))
MincerianEconometriaI$Exper2 = MincerianEconometriaI$Exper*MincerianEconometriaI$Exper

#--------------Graficos de dispersion con GGPLOT2-----------------------------#
#En una regresión minceriana, y en general en las regresiones con ingreso,
#el ingreso suele ponerse en logaritmos debido a su dispersión y la diferencia
#de sus valores en niveles respecto a las otras variables, además de permitir
#un análisis en terminos de semielasticidades

#Edu
ggplot(MincerianEconometriaI, aes(x=edu, y=log(Ingreso))) + geom_point()

+ theme_bw()  #Estilo
+ labs(title="Ingreso vs Educación", x="Educación", y="Ingreso (ln)") #Nombres de titulo y ejes
+ geom_point(shape=23, fill="blue", color="darkred", size=3) #Tipo de punto, al diseño sobre le tipo de grafica suelen encontrarlo en estas funciones

G_Edu = ggplot(MincerianEconometriaI, aes(x=edu, y=log(Ingreso))) + geom_point() + 
  theme_bw() + labs(title="Ingreso vs Educación", x="Educación", y="Ingreso (ln)")


#Exper
G_Exper = ggplot(MincerianEconometriaI[MincerianEconometriaI$Exper != 0, ], aes(x=Exper, y=log(Ingreso))) + geom_point() + 
  theme_bw() + labs(title="Ingreso vs Experiencia", x="Experiencia", y="Ingreso (ln)")


#Genero
G_gen = ggplot(MincerianEconometriaI, aes(x=genero, y=log(Ingreso))) + geom_point() + 
  theme_bw() + labs(title="Ingreso vs sexo al nacer", x="Sexo al nacer", y="Ingreso (ln)")


MultipleG = G_Edu + G_Exper + G_gen

#-----------------------------Modelo--------------------------------#
MinReg = lm(log(Ingreso) ~ edu + genero + Exper + Exper2, data=MincerianEconometriaI)
summary(MinReg)
#Interprete, recuerde que el ingreso está en logaritmos

#--------------Prueba de hipótesis: significancia------------------------------#
#Recuerde que en MCO para realizar inferencia se debe cumplir normalidad en 
#los errores, de manera que nuestros coeficientes distribuyan normales.


#-------Prueba de significancia------#

#Estadístico t H0: B=0
#sigma2est = sum(t(MinReg$residuals)%*%MinReg$residuals)/(nrow(MincerianEconometriaI)-5)
vcov(MinReg)
tstat_edu = MinReg$coefficients[2]/sqrt(vcov(MinReg)[2,2])
qnorm(0.975) #Normal distribution
qt(0.975, (nrow(MincerianEconometriaI)-5)) #t-student distribution

#--------Intervalo de confianza------#
#Si quiero un intervalo de 95% de confianza entonces el %5 de significancia
#se reparte entre dos porciones de 2.5% a cada cola de la dsitribución

tcritico = qt(0.975, (nrow(MincerianEconometriaI)-5))
#Con 0.025 será el mismo porque la t-student es simetrica


LowerB = MinReg$coefficients[2] - tcritico*sqrt(vcov(MinReg)[2,2])
UpperB = MinReg$coefficients[2] + tcritico*sqrt(vcov(MinReg)[2,2])
confint(MinReg)


#--------------------Pruebas de hipotesis en general------------------------#
          
          #Los hombres ganan 20% mas que las mujeres?#

#H0: B1 = 0.2 vs H1: B1 != 0.1

tstats = (MinReg$coefficients[3] - 0.2)/sqrt(vcov(MinReg)[3,3])
qt(0.95, (nrow(MincerianEconometriaI)-5))

