#Introducci?n a R
#Camilo Lozada

remove(list = ls()) #Limpiar entorno

#Definici?n de variables--------------

x <- 1

print(x) #Visualizar
x

msg <- "hello"
msg

x<-1:100
x

x <- c(0.5, 0.6)       ## numeric
x <- c(TRUE, FALSE)    ## logical
x <- c(T, F)           ## logical
x <- c("a", "b", "c")  ## character
x <- c(1+0i, 2+4i)     ## complex

x <- vector("numeric", length = 10) 
x

y <- c(1.7, "a")   ## character
y <- c(TRUE, 2)    ## numeric
y <- c("a", TRUE)  ## character

z<-c(x,y)
z

class(z)

z <- as.numeric(z)
as.logical(x)
as.character(x)

m <- matrix(nrow = 2, ncol = 3) 
m
dim(m)
attributes(m)
m <- matrix(1:6, nrow = 2, ncol = 3) 
m

m <- 1:10 
m
dim(m) <- c(2, 5)
m

x <- 1:3
y <- 10:12
cbind(x, y)
rbind(x, y) 

x <- list(1, "a", TRUE, 1 + 4i) 
x

x <- vector("list", length = 5)
x

x <- factor(c("yes", "yes", "no", "yes", "no")) 
x
table(x)

## See the underlying representation of factor
unclass(x)  

x <- factor(c("yes", "yes", "no", "yes", "no"))
x  ## Levels are put in alphabetical order
x <- factor(c("yes", "yes", "no", "yes", "no"),
            levels = c("yes", "no"))
x


#NA significa que el error ya estaba all? cuando importar la hoja de c?lculo en R. 
#NaN significa que caus? el error despu?s de importar los datos.


## Create a vector with NAs in it
x <- c(1, 2, NA, 10, 3)  
## Return a logical vector indicating which elements are NA
is.na(x)    
## Return a logical vector indicating which elements are NaN
is.nan(x)   
## Now create a vector with both NA and NaN values
x <- c(1, 2, NaN, NA, 4)
is.na(x)
is.nan(x)

x <- data.frame(foo = 1:4, bar = c('D', T, F, F)) 
x
class(x$bar)

nrow(x)
ncol(x)

x <- 1:3
names(x)
names(x) <- c("New York", "Seattle", "Los Angeles") 
x
names(x)

x <- list("Los Angeles" = 1, Boston = 2, London = 3) 
x
names(x)

m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b"), c("c", "d")) 
m

colnames(m) <- c("h", "f")
rownames(m) <- c("x", "z")
m
#2: Bloques básicos --------------

5+7
x <- 5+7
y <- x-3
z <- c(1.1, 9, 3.14)
c(z, 555, z)
z*2 +1000
my_sqrt <- sqrt(z-1)
sqrt(z)
my_div <- z/my_sqrt

c(1, 2, 3, 4) + c(0,10)
c(1, 2, 3, 4) + c(0,10, 100)
my_div

#3: Secuencias de números ----------------

1:20
pi:10
15:1
?':'
seq(1,20)
seq(0, 10, by=0.5)
my_seq<-seq(5, 10, length=30)
length(my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0, times = 40)
rep(c(0, 1, 2), times = 10)
rep(c(0, 1, 2), each = 10)

#4: Vectores-------------

num_vect<-c(0.5, 55, -10, 6) 
tf<- num_vect<1
tf
num_vect >= 6
my_char<-c("My", "name", "is")
my_char
paste(my_char, collapse = " ")
c(my_char, "Camilo")
my_name <- c(my_char, "Swirl")
my_name
paste(my_name,collapse = " ")
paste("Hello", "world!", sep = " ")
paste(1:3, c("X", "Y", "Z"), sep="")
paste(LETTERS, 1:4, sep = "-")

#5: Missing Values------------

x <- c(44, NA, 5, NA)
x*3
y <- rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y, z), 100)
my_na<- is.na(my_data)
my_na
sum(my_na)
my_data

#Cálculos que provocan NaN
0/0
Inf-Inf

#6: Descargar base de datos----------------------------

#Conozcamos Kaggle y otros repositorios

#1. Primero vamos a buscar "Kaggle en nuesttro buscador de preferencia"
#2. Exploremos.
#3. Vamos a la sección de datasets.
#4. Buscamos la base "Student Study Hours"
#5. Descarguemos el dataset y exploremos la base de datos.



