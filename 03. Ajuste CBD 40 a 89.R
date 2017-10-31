
# Ajuste del modelo CBD, para las edades de 40 a 89 años.
# Se utiliza un histórico desde 1975 hasta 2015

# Preparación de datos
#---------------------
edades <-40:89
anyos <-1975:2015

#Se introducen los datos de forma tal que sean reconocidos por la librería StMoMo
#Para ello, se aprovecha el objeto EWMaleData que ya está construído en dicha librería

ESPData=EWMaleData
ESPData$years=anyos
ESPData$ages=edades
ESPData$Dxt=Dth
ESPData$Ext=lx
ESPData$series="total"
ESPData$label="Spain"



# AJUSTE DEL MODELO
#------------------
#Se ajusta el modelo CBD asumiendo  una distribución Binomial, por eso el link es el logit
#lc() identifica que el ajuste a aplicar es el de Lee-Carter

CBDfit_ESP <- fit(link="logit",cbd(), data = ESPData)

#Se obtienen las tasas de mortalidad estimadas por el modelo Lee-Carter mediante la funcion "fitted",
#cuyo argumento es el objeto que contiene el ajuste del modelo.
#rates: qx

CBDmxtHat_ESP <- fitted(CBDfit_ESP, type = "rates")

#Se utiliza un paseo aleatorio bivariante con drift para proyectar las tasas de mortalidad en el horizonte parametrizado (12)
#La función forecast, cuando el argumento es un objeto de la clase "fitStMoMo" por defecto ajusta un paseo aleatorio con drif
#El argumento h indica el número de años que se quieren predecir

CBDfor_ESP=forecast(CBDfit_ESP,h=horizonte)

#Se obtinen las tasas de mortalidad proyectadas

CBDmxtCentral_ESP <- CBDfor_ESP$rates

# Bootstrap: cálculo de la incertidumbre
#---------------------------------------

#Se generan 1000 muestras aleatorios de una Binomial con probabilidades dadas por las que se obtienen al ajustar el modelo a los datos.
#Una vez generadas las muestras aleatorias, se ajusta un modelo Lee-Carter a cada una de ellas,
#obteniéndose los parámetros del modelo para cada una.

CBDboot_ESP=bootstrap(CBDfit_ESP,nBoot=1000,type="semiparametric")

#Con los parámetros obtenidos se utiliza un paseo aleatorio con drift para 
#proyectar las tasas en el horizonte considerado

CBDsimPU_ESP=simulate(CBDboot_ESP,h=horizonte)

#CALCULO DE LOS INTERVALOS DE CONFIANZA AL 99.5%
#Se calculan las curvas (de entre las simuladas anteriormente) tales que entre ellas estan 99.5% de las curvas

#Cálculo de los intervalos para la muestra

CBDmxtHatPU0.05_ESP <- apply(CBDsimPU_ESP$fitted, c(1, 2), quantile, probs = 0.005)
CBDmxtHatPU99.5_ESP <- apply(CBDsimPU_ESP$fitted, c(1, 2), quantile, probs = 0.995)

#Cálculo de los intervalos para las predicciones

CBDmxtPredPU0.05_ESP <- apply(CBDsimPU_ESP$rates, c(1, 2), quantile, probs = 0.005)
CBDmxtPredPU99.5_ESP <- apply(CBDsimPU_ESP$rates, c(1, 2), quantile, probs = 0.995)



