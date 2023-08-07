
## Trabajo Empírico 1
## 05/07/2022


## Librerías

library(astsa)
library(ggplot2)
library(stargazer)
library(kableExtra)
library(knitr)


## setwd("d:/Users/guillermo_reza/Desktop/Maestría/Verano 2022/Macroeconometría Aplicada/Empírico 1")


datos <- read.csv("datos2.csv")




#### Etapa 1: Identificación

par(mfrow = c(2,1))
serie1 <- data.frame(datos$X,datos$x2)
serie2 <- data.frame(datos$X,datos$x3)



## (a) Grafique la serie en niveles.



## Serie x2

grafica1 <- ggplot(serie2, aes(x=datos.X, y=datos.x3))+
  geom_line(color = "blue")


## Serie x3

grafica2 <- ggplot(serie1, aes(x=datos.X, y=datos.x3))+
  geom_line(color = "red")



## (b) Grafique las funciones ACF y PACF. Comentar sobre qué tipo de
## proceso estocástico podría ser el que genera los datos.


## Serie x2

par(mfrow = c(2,1))
acf(serie1$datos.x2, col=2, lwd=3)
pacf(serie1$datos.x2, col=2, lwd=3)

# En esta serie la ACF tiene 3 valores que salen de las bandas y en la parte de 
# PACF tiene 2 componentes autoregresivos. Por lo tanto, los candidatos a 
# ser el modelo que genera el proceso estocástico son:
# ARMA(2,1)
# ARMA(2,2)
# ARMA(2,3)
# ARMA(2,4)
# ARMA(1,1)
# ARMA(2,2)



## Serie x3

par(mfrow = c(2,1))
acf(serie2$datos.x3, col=2, lwd=3)
pacf(serie2$datos.x3, col=2, lwd=3)

# En esta serie la ACF tiene 3 valores que salen de las bandas y en la parte de 
# PACF tiene 3 componentes autoregresivos. Por lo tanto, los candidatos a 
# ser el modelo que genera el proceso estocástico son:
# ARMA(3,1)
# ARMA(3,2)
# ARMA(3,3)
# ARMA(2,3)
# ARMA(2,2)
# ARMA(2,1)




#### Etapa 2: Estimación


## (a) Realizar las estimaciones de los modelos que estén acordes con los
## correlogramas del punto anterior.



### Serie x2


# ARMA(2,1)

s1_fit_21 <- sarima(serie1$datos.x2, p = 2, d = 0, q = 1)

# La parte AR es significativa en los 2 componentes (2), pero la MA no es en ninguno.




# ARMA(2,2)

s1_fit_22 <- sarima(serie1$datos.x2, p = 2, d = 0, q = 2)

# La parte AR es significativa en 1 componente (2), pero la MA no es en ninguno.





# ARMA(2,3)

s1_fit_23 <- sarima(serie1$datos.x2, p = 2, d = 0, q = 3)

# La parte AR es significativa en 1 componente (1) y la parte MA es significativo
# solo en 2 componentes (2 y 3)
# Posible candidato



# ARMA(2,4)

s1_fit_24 <- sarima(serie1$datos.x2, p = 2, d = 0, q = 3)

# La parte AR es significativa en 1 componente (1) y la parte MA es significativo
# solo en 2 componentes (2 y 3)
# Posible candidato, mejor que el ARMA(2,3)




# ARMA(1,1)

s1_fit_11 <- sarima(serie1$datos.x2, p = 1, d = 0, q = 1)

# La parte AR es sginificativa en 1 componente (1) y la parte MA es significativo
# en 1 componente (1)
# El mejor candidato hasta ahora.




# ARMA(2,2)

s1_fit_22 <- sarima(serie1$datos.x2, p = 2, d = 0, q = 2)

# La parte AR es sginificativa en 2 componente (1 y 2) y la parte MA no es significativo
# en ningun componente.






### Serie x3


# ARMA(3,1)

s2_fit_31 <- sarima(serie2$datos.x3, p = 3, d = 0, q = 1)

# La parte AR es significativa en 1 componente (3) y
# la parte de MA es significativa en 1 componente (1).




# ARMA(3,2)


s2_fit_32 <- sarima(serie2$datos.x3, p = 3, d = 0, q = 2)

# La parte AR es significativa en 1 componente (3), pero la MA no es en ninguno.





# ARMA(3,3)


s2_fit_33 <- sarima(serie2$datos.x3, p = 3, d = 0, q = 3)

# La parte AR es significativa en 1 componente (3) y
# la parte de MA es significativa en 1 componente (1).





# ARMA(2,3)

s2_fit_23 <- sarima(serie2$datos.x3, p = 2, d = 0, q = 3)

# La parte AR no es significativa en ningun componente y
# la parte de MA es significativa en 1 componente (3).





# ARMA(2,2)


s2_fit_22 <- sarima(serie2$datos.x3, p = 2, d = 0, q = 2)

# La parte AR es significativa en todos los componentes (1 y 2) y
# la parte de MA es significativa en 2 componentes (1 y 2).
# Candidato más fuerte



# ARMA(2,1)

s2_fit_21 <- sarima(serie2$datos.x3, p = 2, d = 0, q = 1)

# La parte AR es significativa en 1 componente (1) y
# la parte de MA es significativa en 1 componentes (1).
# candidato









## (b) Hacer una tabla con los modelos propuestos que tengan todos sus
## regresores significativos y que muestren residuales ruido blanco.




### Serie x2


## Modelo ARMA (1,1)

stargazer(s1_fit_11$ttable, type = "text", title = "Modelo ARMA(1,1)")

# kbl(as.data.frame(s1_fit_11$ttable), caption = "Modelo ARMA(1,1)", booktabs = T, align = "c") %>%
# kable_styling(full_width = F) 



## Modelo ARMA (2,3)
  
stargazer(s1_fit_23$ttable, type = "text", title = "Modelo ARMA(2,3)")
  

# kbl(as.data.frame(s1_fit_23$ttable), caption = "Modelo ARMA(2,3)", booktabs = T, align = "c") %>%
# kable_styling(full_width = F) 



## Modelo ARMA (2,3)

stargazer(s1_fit_24$ttable, type = "text", title = "Modelo ARMA(2,4)")

# kbl(as.data.frame(s1_fit_24$ttable), caption = "Modelo ARMA(2,4)", booktabs = T, align = "c") %>%
# kable_styling(full_width = F) 










### Serie x3


## Modelo ARMA (2,2)
# Candidato más fuerte

stargazer(s2_fit_22$ttable, type = "text", title = "Modelo ARMA(2,2)")



## Modelo ARMA (2,1)

stargazer(s2_fit_21$ttable, type = "text", title = "Modelo ARMA(2,1)")









#### Etapa 3: Verificación


## (a) Evalúe los residuales de los modelos propuestos, incluyendo su dis-
## tribución, función de autocorrelación parcial, prueba de normalidad
## y estadístico Ljung-Box.




## Serie x2


# ARMA(1,1)

arma11_seriex2 <- s1_fit_11

# Este modelo cumple con los residuos estandarizados porque se distribuyen como 
# Ruido Blanco, en el autocorrelograma se muestra que en el tau(3) sale de las bandas
# junto con el tau (20). En la pureba de normalidad la gran mayoría de los errores
# se encuentra dentro de las bandas y en general se ve que todas las observaciones
# siguen una línea recta. Sin embargo, el problema principal de este modelo es 
# el estadístico Ljung-Box porque en los promeros lags los valores p son significativos
# lo cual hace pensar que los residuales no son independientes.



# ARMA(2,3)

arma23_seriex2 <- s1_fit_23

# En este modelo los residuales están estandarizados y no muestran un comportamiento
# distinto al Ruido Blanco. En el autocorrelograma ninguno de los tau salen de las 
# bandas. Esto sugiere que tiene una mejor especificación del modelo. La prueba
# de normalidad de los residuos siguen la línea recta incluso en los extrmos.
# Finalmente, en el estaístico Ljung-Box no hay ningún valor p que sea significativo
# de esta manera, este modelo parece ser una mejor especificación de la serie x2.



# ARMA(2,3)

arma24_seriex2 <- s1_fit_24

# En este modelo los residuales no tienen una tendencia en los residuos estandarizados.
# En el correlograma todos los tau están dentro de las bandas por lo que tiene
# mejores resultados que el ARMA (1,1). En la gráfica de normalidad los residuales
# se comportan normal y siguen la línea. Con el estadístico Ljung-Box todos los valores p
# son no significativos, solo que el primero está cerca de la significancia. Por lo tanto.
# no se rechaza la hipótesis nula de que los errores son independientes.






## Serie x3


# ARMA(2,1)

arma21_seriex3 <- s2_fit_21


# En este modelo los residuales estandarizados no muestran una tendencia o un patrón
# en la gráfica. En el autodorrelograma muestra dos tau que salen de los límites 
# de significancia, en total son 3 tau que preocupan. Sin embargo, los residuos se 
# comportan de manera normal dentro de las bandas y siguen la línea recta. Con el 
# estadíscito Ljung-Box los residuales se presentan cerca de la región de rechazo
# de la hipótesis nula y los valores p de los dos primeros lag son significativos
# lo que rechaza la hipótesis nula de que los residuales son independientes.



# ARMA(2,1)

arma22_seriex3 <- s2_fit_22


# Este modelo los residuos estandarizados no tienen tendencia ni ningún patrón 
# de comportamiento. Se puede argumentar que es Ruido Blanco. En el autocorrelograma
# solamente el tau 20 sale de los límites y muchos logran ser nulos. Con los
# residuos estandarizados se encuentran dentro de las bandas y siguen la trayectoria
# de la línea recta.Sin embargo, en el estadístico Ljung-Box dos de los residuales 
# provocan que se rechace la hipótesis nula de que los residuos son independientes.







## (b) Realice las pruebas de sobreidentificación de los modelos que consi-
##  dera pueden ser los que generan los datos.




## Serie x2  SOBREIDENTIFICACIÓN


# Se mantienen en el análisis los modelos ARMA(1,1) y ARMA(2,3)



# Modelo ARMA(1,1)

x2_fit1_11 <- arima(serie1$datos.x2,order =c(1,0,1))
x2_fit2_11 <- arima(serie1$datos.x2,order =c(1,0,2))
x2_fit3_11 <- arima(serie1$datos.x2,order =c(1,0,3))
x2_fit4_11 <- arima(serie1$datos.x2,order =c(2,0,1))
x2_fit5_11 <- arima(serie1$datos.x2,order =c(2,0,2))
x2_fit6_11 <- arima(serie1$datos.x2,order =c(2,0,3))


stargazer(x2_fit1_11,x2_fit2_11,x2_fit3_11,x2_fit4_11,x2_fit5_11,x2_fit6_11, type = "text")





## Serie x3 SOBREIDENTIFICACIÓN


# Se mantienen en el análisis los modelos ARMA(2,1) y ARMA(2,2)




# Modelo ARMA(2,2)


x3_fit1_22 <- arima(serie2$datos.x3,order =c(1,0,1))
x3_fit2_22 <- arima(serie2$datos.x3,order =c(1,0,2))
x3_fit3_22 <- arima(serie2$datos.x3,order =c(1,0,3))
x3_fit4_22 <- arima(serie2$datos.x3,order =c(2,0,1))
x3_fit5_22 <- arima(serie2$datos.x3,order =c(2,0,2))
x3_fit6_22 <- arima(serie2$datos.x3,order =c(2,0,3))


stargazer(x3_fit1_22,x3_fit2_22,x3_fit3_22,x3_fit4_22,x3_fit5_22,x3_fit6_22, type = "text")




