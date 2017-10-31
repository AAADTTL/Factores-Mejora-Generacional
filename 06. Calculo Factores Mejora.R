
#FACTORES DE MEJORA
#------------------

#Cálculo de los factores de mejora a través de la mortalidad proyectada

#Se define función auxiliar que calcula los el factor de mejora en un periodo, en este caso se utiliza 2016-2027
FM=function(A)
{m=ncol(A)
fm=(-1)*((A[,m]/A[,1])^(1/m)-1)
return(fm)
}

#A continuación se calculan los factores de mejora por edad para cada uno de los modelos



#LEE-CARTER
#----------
#Primero se unen las qx estimadas por  el modelo Lee-Carter para las edades 40-90 y 91-95
#Información en 2 dimensiones (edad,año)
CC=rbind(mxtCentral_ESP,mxtCentral_ESP_old[51:(edad_max_modelo-39),])

#Se calculan los factores de mejora
FACTOR_MEJORA_LC<-FM(CC)

#Ahora se calcula el estress 99.5% para los factores de mejora, para ello se unen las proyecciones
#simuladas para el modelo Lee-Carter para las edades 40-90 y 91-95
#Información en 3 dimensiones (edad,año,bootstrap)
B=abind(LCsimPU_ESP$rates,LCsimPU_ESP_old$rates[51:(edad_max_modelo-39),,],along=1)

#Se calculan los factores de mejora para cada una de la curvas simuladas 
A=apply(B, 3, FM)

#Se calcula el percentil 99.5 para los Factores de Mejora
A=apply(A,1,quantile, probs=0.995)

#Para la unificación de los factores de mejora con los factores estresados,
#se crea una matriz donde la primera colunma es el factor de
#mejora por edad y la segunda el stress (ambos en porcentaje)
FM_LC_ESP=100*cbind(FACTOR_MEJORA_LC,A)



#CBD
#---
#Primero se obtienen las  qx estimadas por  el modelo CBD
CC=CBDmxtCentral_ESP

#Se calculan los factores de mejora
FACTOR_MEJORA_CBD<-FM(CC)

#Ahora se calcula el estress 99.5% para los factores de mejora
#Se calculan los factores de mejora para cada una de la curvas simuladas 
A=apply(CBDsimPU_ESP$rates, 3, FM)

#Se calcula el percentil 99.5 para los Factores de Mejora
A=apply(A,1,quantile, probs=0.995)


#Para la unificación de los factores de mejora con los factores estresados,
#se crea una matriz donde la primera colunma es el factor de
#mejora por edad y la segunda el stress (ambos en porcentaje)
FM_CBD_ESP=100*cbind(FACTOR_MEJORA_CBD,A)



#P-SPLINE
#--------
#Primero se obtienen las tasas de mortalidad
B=exp(pre.for3$fit)

#Se transforman las tasas en qx
B=1-exp(-B)

#Se obtiene los factores de mejora
FACTOR_MEJORA_PSPLINE=FM(B[,43:53])*100

#Se obtiene los intervalos de confianza para la tasa de mortalidad
CC=exp(pre.for3$fit-2.57*pre.for3$se.fit)

#Se transforman estos intervalos a la qx
CC=1-exp(-CC)

#Se obtiene los factores de mejora esresados
FM_SP99.5_ESP=100*FM(CC[,43:53])


#Para la unificación de los factores de mejora con los factores estresados,
#se crea una matriz donde la primera colunma es el factor de
#mejora por edad y la segunda el stress (ambos en porcentaje)
FM_SP_ESP=cbind(FACTOR_MEJORA_PSPLINE,FM_SP99.5_ESP)



# Factores de mejora medios
#--------------------------
#Cálculo de los factores de mejora medios para los tres modelos para las edades 40-90
CC=(apply(cbind(FM_SP_ESP[,1], FM_LC_ESP[1:50,1],FM_CBD_ESP[,1]),1,mean))

#Se une con los factores de mejora para las edades 91-95 (solo para el modelo Lee-Carter)
FM_MEDIO_ESP_BRUTOS=(c(CC,FM_LC_ESP[51:(edad_max_modelo-39),1]))

#Cálculo de los factores de mejora estresados al 99,5% medios para los tres modelos para las edades 40-90
CC=(apply(cbind(FM_SP_ESP[,2], FM_LC_ESP[1:50,2],FM_CBD_ESP[,2]),1,mean))

#Se une con los factores de mejora estresados para las edades 91-95 (solo para el modelo Lee-Carter)
FM_0.995_ESP_BRUTOS=(c(CC,FM_LC_ESP[51:(edad_max_modelo-39),2]))
