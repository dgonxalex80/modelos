---
title: "Modelo1"
author: "dgonzalez"
date: '2022-05-03'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE,  # no saca mensajes 
                      warning = FALSE,  # no saca mensajes de peliro
                      comment = NA)     # no saca ## en las salidas
#install.packages("TeachingSampling")
library(TeachingSampling)
library(psych)
library(summarytools)
library(gtools)
library(ggplot2)
# colores
c0="#0DA5A6" # VERDE CLARO
c1="#E77C00" # NARANJA
c2="#6666FF" # AZUL  
c3="#4CBFBA" # VERDE CLARO  
c4="#E09600" # AMARILLO  
c5="#BC2B6A" # MORADO  

```

## **Regresión Lineal Múltiple**


```{r}
# importar la data desde formato csv
library(readr)
data=read_delim("data/ausentismo.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

cov(data) # matriz de covarianzas
cor(data) # matriz de correlaciones

# Estimacion del modelo -----
attach(data)
modelo1=lm(ausen ~ taller + sexo + edad + antg + sala , data=data)
summary(modelo1) 

# diagnostico  del modelo ----
coefficients(modelo1) # coeficientes estimados
yhat=fitted(modelo1) # valores estimados
u=residuals(modelo1) # residuales
anova(modelo1) # tabla de anova
vcov(modelo1) # matriz de varianzas covarianza de parametros 


# Stepwise Regression ----
# metodo de eleccion del mejor modelo 
library(MASS)
modelo2=lm(ausen ~ taller + sexo + edad + antg + sala , data=data)
step=stepAIC(modelo2, direction="both")
step$anova # muestra los resultados


modelo3=lm(ausen ~ taller + sexo + antg + sala , data=data)
summary(modelo3)

modelo4=lm(ausen ~ sexo + antg + sala , data=data)
summary(modelo4)

uhat=modelo4$residuals # guarda los residuales
#-----------------------------------------------------------------
# Examen de normalidad de errores
shapiro.test(uhat)

# Supuesto de no autocorrelacion
# install.packages("lmtest")
library(lmtest)
# Prueba de D-W  - autocorrelacion
# Ho: los erreres no estan autocorrelacionados
dwtest(modelo4)

# Supuesto de homoscedasticidad
# Prueba de Goldfeld-Quandt
# Ho no existe heteroscedasticidad
gqtest(modelo4)

# Supuesto de correcta especificacion
# Prueba de especificacion
# Prueba RESET
resettest(modelo4, power=2, type="regressor")

# estimación del mejor modelo
modelo5=lm(ausen ~ antg + sala , data=data)
summary(modelo5)

```
