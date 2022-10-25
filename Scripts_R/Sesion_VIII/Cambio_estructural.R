#CAMBIO ESTRUCTURAL

#----------------------------------------------------------------------------#
#                         Corte transversal     
#----------------------------------------------------------------------------#
library(tidyverse)
library(readxl)
library(plotly)

#Cargar las bases
Ahorro <- read_excel("C:/Users/Shelly G/Downloads/BaseMonEconometria.xls", sheet=1)
FBKF <- read_excel("C:/Users/Shelly G/Downloads/BaseMonEconometria.xls", sheet=2)

#Escoger el año con mas datos
ISNAS = is.na(FBKF)
apply(ISNAS, 2, sum)
#Se escoge 2005

#----------Relación entre ahorro y formación brutal de capital---------------#
#Lo mejor sería que ambas variables estén en la misma base
Base = inner_join(Ahorro[, c(1,3,5)], FBKF[, c(1,3,5)], by=c("Country Code", "Income group"))
colnames(Base) = c("Code", "Group", "Ahorro", "FBKF")
Base = Base %>% drop_na()
#Estimar
Reg1 = lm(FBKF ~ Ahorro, data=Base)
summary(Reg1)


#---Los paises mas desarrollados tienen niveles de ahorro y FBKF mas altos?---#

unique(Base$Group)
Graf = ggplot(Base, aes(x = Ahorro, y = FBKF, color = Group)) +
  geom_point() + theme_bw()
ggplotly(Graf)
which(Base$Ahorro < -18)


#Crear variables categoricas
library(fastDummies)
Base1 = dummy_columns(Base, select_columns = "Group", remove_first_dummy = TRUE)
View(Base1)

Reg2 = lm(FBKF ~ Ahorro + `Group_Low income` + 
            `Group_Lower middle income` + `Group_Upper middle income`, data=Base1)
summary(Reg2)

#Y con 2 grupos?
Base2 = Base %>% 
  mutate(low_income = ifelse(Group == "Lower middle income" |
                               Group == "Low income",1,0))
Reg3 = lm(FBKF ~ Ahorro + low_income, data=Base2)
summary(Reg3)

#La relación entre Ahorro y FBKF es diferente entre paises con alto y bajo
#                         nivel de ingresos?

#Los paises de altos ingresos suelen ser exportadores de ktal, los paises
#en desarollo lo importan, tienen alta IED, aun con niveles de ahorro bajos
#deberiamos esperar una pendiente menos inclinada para paises de bajo ingreso

Reg4 = lm(FBKF ~ Ahorro + `Group_Low income` + 
            `Group_Lower middle income` + `Group_Upper middle income` +
            Ahorro*`Group_Low income` + 
            Ahorro*`Group_Lower middle income` +
            Ahorro*`Group_Upper middle income`, data=Base1)
summary(Reg4)

#--------------------------------------------------------------------------#
#                             Series de tiempo
#--------------------------------------------------------------------------#
#Para ver si el comoprtamiento de una serie afecta la otra es más propicio
#observar las dos series de tiempo una contra la otra

Col <- read_excel("C:/Users/Shelly G/Downloads/AhorroFBKFCol.xls")
Col$Time = as.Date(Col$Time, format = "%Y")
p1 <- ggplot(Col, aes(x=Time, y=Ahorro, group=1)) +
  geom_line() + theme_bw() +scale_x_date(date_labels = "%Y") +
  labs(y="Ahorro")
p2 <- ggplot(Col, aes(x=Time, y=FBKF, group=1)) +
  geom_line() + theme_bw() +scale_x_date(date_labels = "%Y") +
  labs(y="FBKF")
library(patchwork)
p = p1 / p2

#Miremos la regresion inicial
Reg5 = lm(FBKF ~ Ahorro, data=Col)
summary(Reg5)

Col = Col %>% 
  mutate(Crisis = if_else(Time == "1999-10-17" |
                            Time =="2020-10-17", 1, 0))
#Cambio estructural en el nivel?
Reg6 = lm(FBKF ~ Ahorro + Crisis, data=Col)
summary(Reg6)
#Cambio estructural en la pendiente?
Reg7 = lm(FBKF ~ Ahorro + Crisis + Ahorro*Crisis, data=Col)
summary(Reg7)

#-----------------------Test de chow---------------------------#
library(strucchange)
sctest(Reg6)

