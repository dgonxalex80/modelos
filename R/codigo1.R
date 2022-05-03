# Practica 1 ========================================
x=20      # guarda un objeto "x" en memoria
y<-10     # guarda "y" que contiene el valor de 10
x+y       # calcula el valor pero no lo guarda
x=2.1e+2
x=5/0
ls()  # lista los objetos en memoria actualmente

# operadores  aritmeticos    + - * /
3 + 4
3+4*8-2^2

sqrt(2)
log(10)
log10(100)
log2(8)
exp(1)

# VECTORES ------------------------------------
x=c(2,4,3,6,8,7,5,6,9,2)
x=1:10
x=seq(1,19, by=2)
x=seq(0,1, by=0.1)
x=seq(0,1, length=9)
x=seq(0,7, by=pi)
x=rep(1:5,2)
x=rep(1,5)
x=rep(1:5,c(1,2,3,4,5))
x=rep(1:5, each=2)
x=rep(1:5, each=2, len=4)

# operaciones con vectores----------------------

x=c(1,2,3,4,5) # x es un vector
y= 1:5         # otra forma de escribir x

sqrt(x)
sum(x)
cumsum(x)
x*y
x%*%y
xy=c(x,y)
sum(x)/length(x)
mean(x)
sum((x-mean(x))^2)/(length(x)-1)
var(x)

# valores faltantes
s=c(1,2,3,4,5,6,7,8,9,NA)  # NA    no disponible
0/0                        # NaN   No es un numero
x=c(1,2,3,4,NA)
sum(x)
sum(x, na.rm = T)
mean(x, na.rm = T)

s=c(1:5,NA,7:10)
s[!is.na(s)]   # omite el valor NA
s[4:7]

t=-(1:10)

u=c(10,22,32,23)
names(u)=c("Matáticas","Ing.Civil", "Ing. Mecánica","Biología")
u

w=numeric()  # vectpr vacio
w
w[1]=1
w[3]=5
w

# clase de datos
x=1:20
class(x)

y=c("S","N")
class(y)
# matrices ---------------------------------------------
z=matrix(x,nrow = 4)
z
class(z)

z=matrix(x, nrow = 4, byrow = T)
z
dim(z)

z[2:4,2:4]

z[,1]
z[1,]


tipo=c("D","R","B","MB","E")
nivel=sample(tipo,20, replace = TRUE,  prob = c(0.05,0.10,0.30,0.40,0.15))
nivel
nivel=as.factor(nivel)
class(nivel)

nf=c(4.1, 2.7, 3.1, 3.2, 3.0, 3.2, 2.0, 2.4, 1.6, 3.2, 3.1, 2.6, 2.0,
     2.4, 2.8, 3.3, 4.0, 3.4, 3.0, 3.1, 2.7, 2.7, 3.0, 3.8, 3.2, 2.2,
     3.5, 3.5, 3.8, 3.5, 3.9, 4.2, 4.3, 3.9, 3.2, 3.5, 3.5, 3.7, 4.1,
     3.7, 3.5, 3.6, 3.2, 3.1, 3.4, 3.0, 3.0, 3.0, 2.7, 1.7, 3.6, 2.1,
     2.4, 3.0, 3.1, 2.5, 2.5, 3.6, 2.2, 2.4, 3.1, 3.3, 2.7, 3.7, 3.0,
     2.7, 3.0, 3.2, 3.1, 2.4, 3.0, 2.7, 2.5, 3.0, 3.0, 3.0, 3.2, 3.1,
     3.8, 4.1, 3.7, 3.5, 3.0, 3.7, 3.7, 4.1, 3.7, 3.9, 3.7, 2.0)

cc=c(20, 10, 20, 20, 20, 20, 20, 20, 20, 30, 20, 20, 20, 10, 30, 20,
     20, 30, 20, 30, 30, 20, 10, 30, 20, 20, 30, 30, 10, 20, 10, 20,
     20, 20, 10, 20, 10, 20, 20, 30, 30, 30, 10, 30, 20, 20, 20, 20,
     20, 20, 10, 20, 30, 30, 10, 10, 10, 20, 10, 20, 10, 30, 20, 10,
     20, 30, 10, 30, 30, 30, 20, 30, 30, 30, 30, 30, 30, 20, 10, 30,
     10, 20, 20, 10, 20, 20, 20, 20, 10, 20)

data=c(cc,nf)
class(data)

# data frame --------------------------------------------------------------
data=data.frame(cc,nf)
class(data)
dim(data)

data$nf
data[,2]

h=hist(data$nf)
class(h)
h$breaks
h$counts
h$density
h$mids
h$xname
h$equidist

#  arreglos
w=array(1:24, dim = c(3,4,2))
w
dim(w)

z=matrix(1:24, nrow = 4, byrow = T)
t(z)

z*z
z%*%t(z)

A =matrix(c(13, -4, 2, -4, 11, -2, 2, -2, 8), 3, 3, byrow=TRUE)
ev=eigen(A)
ev
ev$values

# funciones ------------------------------------------
 #  argumentos  >>  funcion   >>  resultados
 #  opciones        argumentos por defecto

fx=function(x){1/22*exp(-x/22)}
fx(10)

fxy=function(x,y){48*(x+y)/49}
fxy(1,1)

C=function(n,x){choose(n,x)}
C(5,2)

P=function(n,x){C(n,x)*factorial(x)}
P(5,2)

y=sample(1:6,100, replace = T)
grafica=function(x,color){
        barplot(x,
                col=color, las=1)
        grid()
}

z=table(y)
grafica(z,"red")
w=round(prop.table(z)*100,2)
grafica(w,"blue")











