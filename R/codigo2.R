# Practica 2
# paquees requeridos ----
library(summarytools)
library(psych)
library(tidyverse)

#  importar datos
data(iris)
head(iris)  #  primeros 6 registros
tail(iris)  # untimos 6 registros

iris$Sepal.Length  # variable longitud del sepalo
iris[,1]

iris[,c(1,5)]  # primera y ultima variable

iris[1,] # primer registro
iris[1:10,] # primeros 10 registros

table(iris$Species)
prop.table(table(iris$Species))

#------------------------------------------------------------
# solo correr en equipo , pues este proceso puede tardar mas de 5 minutos
# install.packages("RSocrata")  # instal paquete RSocrata
# library(RSocrata)
# token ="ew2rEMuESuzWPqMkyPfOSGJgE"
# Colombia= read.socrata("https://www.datos.gov.co/resource/gt2j-8ykr.json", app_token = token)
# saveRDS(Colombia,"Colombia.RDS")

Colombia=readRDS("data/Colombia.RDS")
#------------------------------------------------------------
table(Colombia$ubicacion)

# Arreglo de variables -------------------------------------
# se debe instalar previamente el paquete stringr
library(stringr)
Colombia$sexo=str_to_lower(Colombia$sexo)
Colombia$estado[Colombia$estado=="N/A"]="NA"
Colombia$estado=str_to_lower(Colombia$estado)
Colombia$recuperado[Colombia$recuperado=="N/A"]="NA"
Colombia$recuperado=str_to_lower(Colombia$recuperado)
Colombia$fuente_tipo_contagio[Colombia$fuente_tipo_contagio=="N/A"]="NA"
Colombia$fuente_tipo_contagio=str_to_lower(Colombia$fuente_tipo_contagio)
Colombia$ubicacion[Colombia$ubicacion=="N/A"]="NA"
Colombia$ubicacion=str_to_lower(Colombia$ubicacion)
#--------------------------------------------------
cc=c(20, 10, 20, 20, 20, 20, 20, 20, 20, 30, 20, 20, 20, 10, 30, 20, 20, 30,
     20, 30, 30, 20, 10, 30, 20, 20, 30, 30, 10, 20, 10, 20, 20, 20, 10, 20,
     10, 20, 20, 30, 30, 30, 10, 30, 20, 20, 20, 20, 20, 20, 10, 20, 30, 30,
     10, 10, 10, 20, 10, 20, 10, 30, 20, 10, 20, 30, 10, 30, 30, 30, 20, 30,
     30, 30, 30, 30, 30, 20, 10, 30, 10, 20, 20, 10, 20, 20, 20, 20, 10, 20)
labs=c("Ing. Industrial","Administración ","Contaduría ")
pct=round(table(cc)/sum(table(cc))*100)
labs=paste(labs, pct)
labs=paste(labs, "%", sep = " ")
pie(table(cc), labels=labs, main=" Distribución por carrera")
#------------------------------------------------
ev=table(rbinom(90,5,0.80))
barplot(ev,
        col=c("red","yellow","orange","green","blue"),
        main = "Evaluación proceso de inducción")
#-------------------------------------------------
counts= table(mtcars$vs, mtcars$gear)
rownames(counts)=c("Montor en linea", "Motor en V")
barplot(counts,
        main="Número de cambios adelante por Tipo de motor",
        xlab="Número de cambios adelante ",
        col=c("dodgerblue3","orange"),
        legend = rownames(counts))




#------------------------------------------------
nf=c(4.1, 2.7, 3.1, 3.2, 3.0, 3.2, 2.0, 2.4, 1.6, 3.2, 3.1, 2.6, 2.0, 2.4, 2.8,
     3.3, 4.0, 3.4, 3.0, 3.1, 2.7, 2.7, 3.0, 3.8, 3.2, 2.2, 3.5, 3.5, 3.8, 3.5,
     3.9, 4.2, 4.3, 3.9, 3.2, 3.5, 3.5, 3.7, 4.1, 3.7, 3.5, 3.6, 3.2, 3.1, 3.4,
     3.0, 3.0, 3.0, 2.7, 1.7, 3.6, 2.1, 2.4, 3.0, 3.1, 2.5, 2.5, 3.6, 2.2, 2.4,
     3.1, 3.3, 2.7, 3.7, 3.0, 2.7, 3.0, 3.2, 3.1, 2.4, 3.0, 2.7, 2.5, 3.0, 3.0,
     3.0, 3.2, 3.1, 3.8, 4.1, 3.7, 3.5, 3.0, 3.7, 3.7, 4.1, 3.7, 3.9, 3.7, 2.0)

# diagrama de tallos y hojas
stem(nf)

# histograma
hist(nf)

h1=hist(nf,
        main = "Nota final matemáticas fundamentales",
        xlab = "nota",
        ylab="frecuencias absolutas",
        labels=TRUE,
        col="dodgerblue3", ylim = c(0,30))
abline(v=3,col="red")

# diagrama de dispersion
ed=round(rnorm(90,18,1),1)   # variable edad simulada
plot(ed,nf,
     main="Edad vs Nota final matemáticas fundamentales",
     ylim = c(0,5),
     xlab = "Edad",
     ylab = "Nota final",
     col="dodgerblue3",
     pch=19)
grid()
#--------------------------------------------------
#  diagrama de cajas
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Length~iris$Species)
boxplot(iris$Sepal.Length~iris$Species,
        xlab="Especie",
        ylab="Longitud del sepalo",
        main="Comparación de especies", col=c("red","blue","yellow" ),
        las=1)
#---------------------------------------------------
#  Grafoca de serie de tiempo
plot(AirPassengers,    # data set de R
     main="Numero de pasajeros por mes",
     col="dodgerblue3", lwd = 2)
#-----------------------------------------------------
# Resumen de graficos
x=rnorm(100,100,20)
y=rnorm(100,100,25)
z=rbinom(100,4,0.30)
t=1:100
pie(table(z))
barplot(table(z))
stem(x)
hist(x)
boxplot(x)
plot(x,y)
plot(t,y, type="l")
plot(density(x))

#----------------------------------------------
summary(iris$Sepal.Length)
tapply(iris$Sepal.Length,iris$Species, mean)
tapply(iris$Sepal.Length,iris$Species, summary)

psych::describe(iris$Sepal.Length)
summarytools::descr(iris$Sepal.Length)
#-------------------------------------------

