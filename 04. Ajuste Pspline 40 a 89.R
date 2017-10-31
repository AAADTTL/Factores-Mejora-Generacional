
# Ajuste del modelo Pspline, para las edades de 40 a 89 años.
# Se utiliza un histórico desde 1975 hasta 2015

# Preparación de datos
#---------------------
edades <-40:89
anyos <-1975:2015

#Se compila la función my.predict, que permite el cálculo de los intervalos de confianza para las predicciones.
source("my.predict.R")



# AJUSTE DEL MODELO
#------------------
#Se ajusta el modelo P-spline bidimensional con 10 nodos para la edad y 11 para el año
#En este caso se está trabajando con la distribución de Poisson
#Para ajustar el modelo se utiliza la funcion "Mort2Dsmooth" cuyos argumentos son las edades,
#los años y la matriz de defunciones y población estacionaria.
#Es necesario elegir el número de nodos para la edad y los años. Entre 10 y 15 suele ser generalmente lo correcto.
#Death es la matriz de defunciones, dimensiones edad x años
#Exposure es la matriz población estacionaria, dimensiones edad x años

fitBIC3 <- Mort2Dsmooth(x=edades, y=anyos, Z=Dth ,offset=log(Exp),ndx=c(10,11))

#Cálculo de la incertidumbre
#---------------------------------------

##Se definen el rango de años para predecir
prediccion <- 1975:(2015+horizonte)

#Creación del nuevo conjunto de datos sobre el que hacer las predicciones
nuevos_datos <- list(x=edades, y=prediccion) 

#Calculamos las predicciones con las funciones predict y my.predict, la primera se utiliza para obtener
#la predicción en si y la otra para obtener el error est?ndard de la predicci?n
#El argumento se.fit nos permite objetener los errores estandar para el predictor lienal
pre.for3 <- my.predict(fitBIC3, newdata=nuevos_datos, se.fit=TRUE)
