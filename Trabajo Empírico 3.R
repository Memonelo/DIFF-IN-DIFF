

## Trabajo Empírico 
## Guillermo Xicohténcatl Reza
## 19/07/2022



## Dirección
## setwd("d:/Users/guillermo_reza/Desktop/Maestría/Verano 2022/Macroeconometría Aplicada/Empírico 3")



## Librerías
library(stats)
library(forecast)
library(urca)
library(tidyverse)
library(stargazer)
library(astsa)
library(fBasics)
library(fGarch)
library(FinTS)
library(quantmod)


## Lectura de datos

serie <- read.csv("TE3.csv")





######### P A R T E _ 1 ######### 


##### a) Verificación de serie y estadísticos descriptivos. 


serie1 <- serie$serie1


plot(serie1, type="l", col="red")



# Para verificar que la serie sea estacionaria se debe rechazar la prueba de
# Dicky Fuller. 



prueba_df_serie1 <- ur.df(serie1, type = "trend", lags = 0, selectlags = "AIC")

summary(prueba_df_serie1)


# De la prueba de Dickey Fuller se obtiene que el estadístico de la serie 
# es -18.9614, el cual se encuentra a la izquierda en la distribución de -3.98.
# Esto indica que la hipótesis de la prueba se rechaza, por lo tanto, no hay
# raíz unitaria y la serie es estacionaria.




## Análisis descriptivo de datos

summary(serie1)

## Datos
Serie_1 <- c("valor")
Datos <- class(serie1)
Frecuencia <- frequency(serie1)
Inicio <- start(serie1)
Final <- end(serie1)
Min <- -5.5128
Max <- 2.6168
Prom <- -0.8381
observaciones <- length(serie1)




## Construcción de la tabla
tabla_1 <- rbind(Serie_1,Datos,Frecuencia,
                 Inicio,Final,Min,Max,Prom,observaciones)

stargazer(tabla_1, type = "text", title = "Estadísticas Descriptivas")







##### b) Estimar el modelo yt = mu + et


modelo1 <- lm(serie1~1, serie)

stargazer(modelo1, type = "text", title = "Modelo 1")






##### c) GARCH con la serie et al cuadrado


## Construcción de la serie de residuales al cuadrado

checkresiduals(modelo1)

# En el autocorrelograma no parece tener valores fuera ed las bandas
# por lo que se piensa que no hay una estructura de varianza heteroscedástica. 
# Sin embargo, en la distribución de los errores al cuadrado no sigue una 
# distribución normal. En la gráfica de los residuales parece que la varianza 
# es constante.



# Se crea el modelo de los errores al cuadrado para evaluar la estructura de
# varianza heteroscedástica.

epsilon2 <- resid(modelo1)^2





## Inspección Gráfica


plot(epsilon2, main = "Residuales al cuadrado", type="l", col="red")

# En algunos espacios de la gráfica se ven saltos muy superiores a los que están
# en otros. Por ejemplo los saltos del periodo cercano a 100 son muy superiores 
# a los saltos en el lugar 50. Esto es evidencia de que la varianza no es 
# heteroscedástica. Se procede a observar el autocorrelograma y el correlograma
# parcial.


## Autocorrelación serial
par(mfrow = c(2,1))
acf(resid(modelo1),lag.max = 500)
pacf(resid(modelo1),lag.max = 500)

# En la lectura del autocorrelograma se puede observar que el primer tau es el
# que sale claramente de las bandas, pero eso es la varianza de los errores
# entre la varianza de los errores, por lo tanto, siempre será igual a 1. 
# Es el único de los tau que sale de las bandas claramente. Para los siguientes
# periodos se muestran afuera algunos cercanos al lugar 25 o 30. Esto indica
# la posible estructura de medias móviles (MA) en los errores. 

# En el autocorrelograma parcial que se revisa la posible estructura
# autorregresiva se puede observar que en el lugar 45 y en el 50 hay valores que
# salen de las bandas. Esto da evidencia de la presencia de una estructura 
# autorregresiva en los errores y se debe verificar con las siguientes pruebas.




ArchTest(epsilon2, lags = 1, demean = TRUE)

## La prueba para tener información sobre la presencia de una estructura GARCH
## dá como resultado un p-value de 0.7171 con la hipótesis nula que no tienen
## efectos ARCH, entonces se rechaza la hipótesis nula y se concluye que 
## el modelo no tiene una varianza constante.


## Por el autocorrelograma se puede observar que los tau que salen de las bandas
## se encuentran cerca del periodo 50. Por lo tanto, no se considera que tenga 
## efectos de media móvil. Solo podríamos considerar los componentes 
## autorregresivos.




##### d) Estimación del modelo ARMA + GARCH



# Partimos de la estimación del modelo ARMA para saber el número de
# componentes de media móvil y de autorregresivos que tiene la serie.
# Para esto utilizamos los métodos gráficos del autocorrelograma
# y el autocorrelograma parcial.


# Serie 1: Autocorrelograma y Autocorrelograma Parcial

serie1 <- ts(serie1)

par(mfrow = c(2,1))
acf(serie1)
pacf(serie1)

# En los autocorrelogramas no se muestran componentes autorregresivos ni de 
# media móvil. Sin embargo, se propone un modelo ARMA(1,1) para ver los residuos
# y evaluar la presencia de heteroscedasticidad.

# Se proponen dos modelos: ARMA(0,0) y ARMA(1,1)

modelo00 <- sarima(serie1, p=0, d=0, q=0)


modelo11 <- sarima(serie1, p=1, d=0, q=1)

# Ahora se considera que la varianza es heteroscedástica con un GARCH(0,1)
# para el modelo ARMA(0,0)

modelo001<-garchFit(~arma(0,0)+garch(1,1),
                   data=serie1,trace = FALSE)


summary(modelo001)



# Ahora se considera que la varianza es heteroscedástica con un GARCH(0,1)
# Para el modelo ARMA(1,1)

modelo111<-garchFit(~arma(1,1)+garch(1,1),
                    data=serie1,trace = FALSE)


summary(modelo111)




stargazer(modelo001,modelo111, type = "text")


# El mejor modelo es el ARMA(0,0) + GARCH(0,1)



stargazer(modelo001, type = "text")


##### e) Estimación del modelo ARMA + GARCH


modelo001 <- garchFit(~arma(0,1)+garch(1,0),
                   data=serie1,trace = FALSE)

tabla_1 <- summary(modelo10)

plot(modelo001)

# La distribución de los errores del modelo son ruido blanco, en el
# autocorrelograma se muestra que ninguno de los tau son significativos
# en generla, en todos los estadísticos muestra que los residuales
# son independientes y se distribuyen como ruido blanco. De esta forma
# se puede asegurar que el modelo elegido es una buena aproximación de la 
# serie que genera los datos.




##### f) Interpretación del modelo ARMA + GARCH


# El intercepto mu tiene significancia pero el término de media móvil no
# con el coeficiente de la parte GARCh son significativos. Por lo tanto, 
# este modelo ajusta a la serie. 





######### P A R T E _ 2 ######### 




## Se trabajará con la serie de datos de IBM desde el 1 de enero de 2015 hasta
## el 31 de julio de 2015.


getSymbols("IBM",from="2015-01-01", to="2015-07-31"
           ,auto.assign=TRUE,src='yahoo')



# Gráfica de la serie de precios de IBM

plot(IBM$IBM.Adjusted, main = "Precio Ajustado")




## Se definen los retornos de la serie de IBM para el periodo de estudio.
## Se trabajará con la serie de la tasa de crecimiento de la serie. Es decir,
## los rendimientos de la serie.

rendimientos <- dailyReturn(IBM$IBM.Adjusted)



## Gráfica de los rendimientos

chart_Series(rendimientos, name = "Rendimiento del precio ajustado")






##### a) Verificación de serie y estadísticos descriptivos. 



# Para verificar que la serie sea estacionaria se debe rechazar la prueba de
# Dicky Fuller, que supone que hay raíz unitaria y random walk.



prueba_df_IBM <- ur.df(rendimientos, type = "trend", lags = 0, selectlags = "AIC")

summary(prueba_df_IBM)

# De la prueba de Dickey Fuller se obtiene que el estadístico de la serie 
# es -13.3531, el cual se encuentra a la izquierda en la distribución de -3.99.
# Esto indica que la hipótesis de la prueba se rechaza, por lo tanto, no hay
# raíz unitaria y la serie es estacionaria.




## Análisis descriptivo de datos

summary(rendimientos)

## Datos
Serie_IBM <- c("valor")
Datos <- class(rendimientos)
Frecuencia <- frequency(rendimientos)
Inicio <- start(rendimientos)
Final <- end(rendimientos)
Min <- -0.0585959
Max <- 0.0341698
Prom <- 0.0001299
observaciones <- length(rendimientos)




## Construcción de la tabla
tabla_IBM <- rbind(Serie_IBM,Datos,Frecuencia,
                 Inicio,Final,Min,Max,Prom,observaciones)

stargazer(tabla_IBM, type = "text", title = "Estadísticas Descriptivas")



##### b) Estimar el modelo ARMA



par(mfrow = c(2,1))
acf(rendimientos,lag.max = 500)
pacf(rendimientos,lag.max = 500)


# Por el resultado del autocorrelograma, la mayoría de los tau están dentro de 
# las bandas por lo que podemos decir que son no significativos, excepto el 
# del segundo periodo. Este parece salir de las bandas.

# Por el lado del autocorrelograma parcial, el segundo término sale de las
# bandas, esto sugiere que la serie tiene un componente autoregresivo.
# Por los autocorrelogramas, se evalúa un modelo ARMA(2,1) y ARMA(2,0)



## Modelo propuesto


# ARMA(2,1)

modelo_IBM_21 <- sarima(rendimientos, p = 2, d = 0, q = 1)


# Este modelo cumple con que el autocorrelograma de los residuales están
# por debajo de las bandas. No son significativos y cuando se grafican, los 
# residuales siguen la trayectoria de una  línea recta. Por parte de los
# valores p en el estadístico de Ljung-Box ninguno es significativo, lo cual
# muestra que los errores son independientes. Sin embargo, en la gráfica de los
# residuales, parece que la varianza es heteroscedástica porque en algunos
# periodos es diferente, es mayor.


modelo_IBM <- arima(rendimientos, order = c(2,0,1))





##### c) GARCH con la serie et al cuadrado


## Inspección Gráfica y Descripción de los residuales

checkresiduals(modelo_IBM)


# Los residuales parecen tener una distribución normal, además en el
# autocorrelograma no hay ninguno significativo. Sin embargo, en la gráfica de
# los residuales parecen tener diferente varianza en distintos momentos.


residuales_IBM <- residuals(modelo_IBM)^2

plot(residuales_IBM, main = "Residuales de IBM al cuadrado")


# En los primeros 20 periodos se muestra una varianza mayor que en los
# siguientes 20 datos. En los siguientes 40 periodos se mantiene este patrón.
# Es por esto que se debe revisar si hay una estructura ARCH o varianza
# heteroscedástica.


par(mfrow = c(2,1))
acf(resid(modelo_IBM),lag.max = 500)
pacf(resid(modelo_IBM),lag.max = 500)


# En los autocorrelogramas y en el autocorrelograma parcial, no se muestra
# que tenga una estructura ARCh en los errores.



## Prueba ARCH

ArchTest(rendimientos, lags = 2, demean = TRUE)

# Como el p-value es 0.95 no se rechaza la hipótesis nula que significa que
# no hay presencia de efectos ARCH.








##### d) Estimación del modelo ARMA + GARCH



# El modelo óptimo es ARMA(2,1)+ GARCH(1,1)

modelo_IBM_optimo <- garchFit(~arma(2,1)+garch(1,1),
                              data=rendimientos,trace = FALSE)







##### e) Sobreidentificación y errores estimados



## El resumen de los datos encontrados

summary(modelo_IBM_optimo)



## Gráficas de los modelos

plot(modelo_IBM_optimo)





## Sobreidentificación

modelo_IBM_optimo <- garchFit(~arma(2,1)+garch(1,1),
                              data=rendimientos,trace = FALSE)


modelo_IBM_1 <- garchFit(~arma(2,1)+garch(2,1),
                              data=rendimientos,trace = FALSE)


modelo_IBM_2 <- garchFit(~arma(2,1)+garch(3,1),
                              data=rendimientos,trace = FALSE)


modelo_IBM_3 <- garchFit(~arma(2,1)+garch(1,2),
                              data=rendimientos,trace = FALSE)


modelo_IBM_4 <- garchFit(~arma(2,1)+garch(1,3),
                              data=rendimientos,trace = FALSE)



stargazer(modelo_IBM_optimo,modelo_IBM_1,modelo_IBM_2,
          modelo_IBM_3,modelo_IBM_4,type = "text")





## Del último modelo se puede ver que resulta ser mejor que el propuesto.
## Ahora revisaremos los residuales para revisar si es mejor.




## El resumen de los datos encontrados

summary(modelo_IBM_4)



## Gráficas de los modelos

plot(modelo_IBM_4)


## No resulta ser mejor que el propuesto, son parecidos pero por el criterio
## AIC y el BIC se prefiere el modelo 4: ARMA(2,1)+ GARCH(1,3)





##### f) Interpretación de los parámetros del modelo.


## Los residuales se consideran como heteroscedásticos con los rendimientos
## de la serie de IBM. Tienen una varianza no constante y el modelo que mejor
## ajusta estas variaciones es ARMA(2,1)+ GARCH(1,3) por tener mejores 
## criterios de información. Además, de mostrar resultados muy parecidos
## en el autocorrelograma y el autocorrelograma parcial. Se tiene con un
## modelo que ajusta la serie de rendimientos bajo los criterios vistos en
## clase.















