# Practica 3
library(mvtnorm)
library(lattice)
library(cubature)
library(scatterplot3d)
library(RSocrata)
library(stringr)
library(BSDA)

# Modulo 0 -Prerrequisitos
# sumatoria
sum(1:100)
x=c(1,2,3,4,5,6,7,8,9,NA)
sum(x)
sum(x,na.rm = T)
cumsum(x[!is.na(x)])  # suma acumulada

# Tecnicas de conteo
C=function(n,k){choose(n,k)}
P=function(n,k){choose(n,k)*factorial(k)}

library(gtools)
N=4  # Número de elementos
n=2  # grupos de 2 en 2
id=c(1:N)
permutations(N, n, id)
P(4,2)

N=5  # Número de elementos
n=2  # grupos de 2 en 2
id=c(1:N)
combinations(N, n, id)
C(5,2)

# urna con 3 bolas
x <- c('Rojo', 'Azul', 'Verde')
permutations(n=3,r=2,v=x,repeats.allowed=TRUE)

#-------------------------------------------------------------
# Grafico de funciones

f=function(x){x/2}
x1=c(-0.1,2.1)
x2=c(0,1)
plot(x2~x1,type="p", xlab="x", ylab="y", col="white")
grid()
curve(f,0,2, add=TRUE, lwd=2)
t=seq(0,1,by=0.01)
x=c(0,t,1)
y=c(0,f(t),0)
polygon(x,y,col="#0000ff22")
abline(h=0,v=0)
#------------------------------------------------------------

f=function(x){1/22*exp(-x/22)}
x=1:100
fx=f(x)
plot(x,fx,
     type="l",
     main="Tiempo de reparación máquina",
     xlab = "tiempo en minutos",
     ylab= "f(x)", col="blue", lwd=4)
grid()
#------------------------------------------------------------

library("mvtnorm")
N=50
x <- seq(-3,3, length=N)
y <- seq(-3,3,length=N)
z <- matrix(0, N, N)
for (i in 1:N) for (j in 1:N) {
  z[i,j]=dmvnorm(c(x[i],y[j]), c(0,0),
                 matrix(c(1,0.5,0.5,1),2,2))}
persp(x,y,z,theta=50, phi=10,
      xlab=" ",
      ylab=" ",
      zlab=" ",
      scale=TRUE,
      expand=.4,
      axes=FALSE)
#------------------------------------------------------------

library(scatterplot3d)
x=c(0,1,1,0,0)
y=c(0,0,1,1,0)
z=c(0,1,2,1,0)

s=scatterplot3d(x,y,z, type='l',
                xlim=c(0,1),ylim=c(0,1),zlim=c(0,2),
                angle=45,xlab="x", ylab="y", zlab="z",
                scale.y=0.4, grid = FALSE,label.tick.marks=TRUE)
x0=c(0,1,1)
y0=c(0,0,0)
z0=c(0,0,1)
polygon(s$xyz.convert(x0,y0,z0),col="#8080FF99")

x1=c(0,1,0)
y1=c(0,0,1)
z1=c(0,1,1)
polygon(s$xyz.convert(x1,y1,z1),col="#8080FF99")

x2=c(0,0,0)
y2=c(0,1,1)
z2=c(0,0,1)
polygon(s$xyz.convert(x2,y2,z2),col="#8080FF99")

x3=c(0,1,0)
y3=c(0,0,1)
z3=c(0,0,0)
polygon(s$xyz.convert(x3,y3,z3),col="#8080FF99")
#----------------------------------------------------------

library(lattice)
x=seq(3,4,by=0.02)
y=seq(0.5,1,by=0.02)
fun=function(x,y){48*x*y^2/49}
z=outer(x,y,fun)
wireframe(z,xlab="x",ylab="y",col="blue")

#----------------------------------------------------------
# Integracion

f=function(x){x/2}
curve(f,0,2) # dibuja linea de la función
abline(h=0,v=0) # traza eje x y eje y
t=seq(0,1,by=0.01)
x=c(0,t,1)
y=c(0,f(t),0)
polygon(x,y,density=30, col="#8080FF99") # pinta área sombreada
p=integrate(f,0,2)
p$value # resultado
#------------------------------------------------------------------------
# Ingeral doble
library(cubature)
f=function(x){3/2*(x[1]^2 +x[2]^2)}
adaptIntegrate(f,lowerLimit=c(0,0),
               upperLimit=c(1,1))
#-------------------------------------------------------------------------
# Integracion triple
library(cubature)
f=function(x){2/3*(x[1]+x[2]+x[3])}

adaptIntegrate(f,lowerLimit=c(0,0,0),
               upperLimit=c(0.5,0.5,0.5))
#-------------------------------------------------------------------------
# Este codigo puede tardar mas de 5 minutos en el proceso
library(RSocrata)
library(stringr)

# https://www.datos.gov.co/Educaci-n/Tasa-Repitencia-Educacion/3gm7-ubmj

token ="ew2rEMuESuzWPqMkyPfOSGJgE"

sabar11=read.socrata("https://www.datos.gov.co/resource/rnvb-vnyh.json", app_token = token)
# Repitencia= read.socrata("https://www.datos.gov.co/resource/3gm7-ubmj.json", app_token = token)
# Colombia= read.socrata("https://www.datos.gov.co/resource/gt2j-8ykr.json", app_token = token)
Colombia$sexo=str_to_lower(Colombia$sexo)
Colombia$estado[Colombia$estado=="N/A"]="NA"
Colombia$estado=str_to_lower(Colombia$estado)
Colombia$recuperado[Colombia$recuperado=="N/A"]="NA"
Colombia$recuperado=str_to_lower(Colombia$recuperado)
Colombia$fuente_tipo_contagio[Colombia$fuente_tipo_contagio=="N/A"]="NA"
Colombia$fuente_tipo_contagio=str_to_lower(Colombia$fuente_tipo_contagio)
Colombia$ubicacion[Colombia$ubicacion=="N/A"]="NA"
Colombia$ubicacion=str_to_lower(Colombia$ubicacion)
saveRDS(Colombia,"Colombia.RDS")
#--------------------------------------------------------------------------

c1="#002147" #
c2="#325d8c" #
c3="#6994bc" #
c4="#95b9db" #
c5="#c4d5f2" #
par(cex=0.8, cex.axis=1, cex.lab=1, cex.main=1, cex.sub=1)
t=table(Colombia$ubicacion) # table en frecuencias abs
t=prop.table(t)*100         # tabla en porcentaje
t=round(t,2)                # tabla en porcentaje 2 decimales
labs=names(t)               # nombres de las categorias
barplot(t,main=" Distribución contagiados de Covid-19 por ubicación",
        ylim = c(0,100),
        xlab ="Ubicación",
        ylab="Porcentaje",
        beside=T,
        col=c(c1,c2,c3,c4,c5),las=1)

legend("topright", inset = 0.1,labs,fill =c(c1,c2,c3,c4,c5),cex =1)

#---------------------------------------------------------------------------
# Problema de los dados

sample(1:6)
sample(1:6, 20, replace = TRUE)

# Suma de dos dados
dd=sample(1:6, 20, replace = TRUE)
mdd=matrix(dd,ncol = 2)
apply(mdd, 1,sum)

#-*****---------------------------------------------------------------------
n=5
dd=sample(1:6, n*2, replace = TRUE)
mdd=matrix(dd,ncol = 2)
sdd=apply(mdd, 1,sum)
barplot(table(sdd), las=2)
prop.table(table(sdd))

#------------------------------------------------------------------------
# Modelos especiales
# Distribucion binomial n=10, p=0.70
x=0:10
fx=dbinom(x,10,0.70)
plot(x,fx, type="p", pch=19, col="orange",las=1, cex=1.5)
grid()

fx=pbinom(x,10,0.70)
plot(x,fx, type="s", pch=19, las=1)
points(x, fx, pch=19, col="orange", cex=1.5)
grid()

#------------------------------------------------------------------------
# Distribicion normal
x=seq(-4,4,by=0.1)
fx=dnorm(x)
plot(x,fx,type = "l", las=1, lwd=2.5, col="red")
grid()

fx=pnorm(x)
plot(x,fx,type = "l", las=1, lwd=2.5, col="red")
grid()
#----------------------------------------------------------------------
# Simulacion

X2=rnorm(1000,mean=2.0,sd=0.1)    #  generación de numeros aleatorios  de X
Y2=rnorm(1000,mean=3.0,sd=0.2)    #  generacion de numeros aleatorios  deY
Z2=data.frame(X2,Y2)              #  generacion de matriz de X,Y

A2=apply(Z2,1,prod)               #  area de la placa A=XY

mediaA=mean(A2)                   #  media del vector de areas
varianzaA=var(A2)                 #  varianza del vector de areas

B2=as.numeric(A2>5.9 & A2<6.1)    #  generacion de variable de 0,1,
                                  #  con 1 donde cumplecondicion
Pro3c=sum(B2)/1000                #  calculo de la  probabilidad

hist(A2)                          # histograma del valor de las areas
plot(density(A2))                 # grafico de la distribucion empirica de A2
qqnorm(A2)                        # grafico QQ de A2
summarytools::descr(A2)
#-------------------------------------------------------------------------
# Correlacion

gen.corr.data<- function(rho,n){
  x <- rnorm(n);
  z <- rnorm(n);
  y<- rho*x + sqrt(1-rho^2)*z ; result <-cbind(y,x)
  return(result)
}
par(mfrow = c(2, 2)) # matriz de graficos 2x2
muestra<-gen.corr.data(0,200)
plot(muestra, pch=19, main = "Correlació Rho=0")
grid()

muestra<-gen.corr.data(0.7,200)
plot(muestra, pch=20, main = "Correlació Rho=0.7")
grid()

muestra<-gen.corr.data(0.9,200)
plot(muestra, pch=20, main = "Correlació Rho=0.9")
grid()

muestra<-gen.corr.data(-0.95,200)
plot(muestra, pch=20, main = "Correlación Rho=-0.95")
grid()

#------------------------------------------------------------------------
# Teorema Central del Limite

n=200 ; m=1000*n

# distribucion exponencial-------------------------------
X=matrix(rexp(m,1),ncol=n)

# generacion de muestras---------------------------------
X1=X[,1]
X2=X[,1:2]
X20=X[,1:20]
X200=X[,1:200]

# generacion de medias-----------------------------------
Mx2=apply(X2,1,mean)
Mx20=apply(X20,1,mean)
Mx200=apply(X200,1,mean)

# histogramas  de comparacion--------------------------
par(mfrow=c(2,2),cex=0.8, cex.axis=1, cex.lab=1, cex.main=1, cex.sub=1, las=1)
hist(X1,  main = "n=1", freq=FALSE)
hist(Mx2, main ="n=2", freq=FALSE)
hist(Mx20, main = "n=20",freq=FALSE)
hist(Mx200, main = "n=200", freq=FALSE)
#-----------------------------------------------------
# Pruebas de hipotesis

# Ho: mu <= 10
# Ha: mu > 10

x=c(11.1, 15.6, 11.1,  7.5,  7.9, 14.7,  6.3,  8.5,  8.0 , 7.6)
t.test(x,
       alternative = "less",
       mu = 10,
       conf.level = 0.95)

# -----------------------------------------------------
# Prueba de normalidad
x=rnorm(200,100,20)
shapiro.test(x)
#------------------------------------------------------
# Prueba no parametrica de Signos
# install.packages("BSDA")
library(BSDA)
# Me <= 15
# Me > 15
x=c(16,15,12,17,18,14,16,14,16,17,19,16,14,21,20,16,16,16)
SIGN.test(x,md=15,alternative = "greater")
#------------------------------------------------------
























