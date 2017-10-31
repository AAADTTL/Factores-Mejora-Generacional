
#Ajuste del modelo Lee-Carter, para las edades de 90 a 95 años.
#Se utiliza un histórico desde 1975 hasta 2015
#El ajuste se realiza desde 40 hasta 95 años, aunque finalmente los resultados seleccionados son los que corresponde a las edades
#de 90 a 95 años.

# Preparación de datos
#---------------------
edades <-40:edad_max_modelo
anyos <-1991:2015

#Se introducen los datos de forma tal que sean reconocidos por la librería StMoMo
#Para ello, se aprovecha el objeto EWMaleData que ya está construído en dicha librería

ESPData=EWMaleData
ESPData$years=anyos
ESPData$ages=edades
ESPData$Dxt=Dth_total
ESPData$Ext=lx_total
ESPData$series="total"
ESPData$label="Spain"



# AJUSTE DEL MODELO
#------------------
#Se ajusta el modelo Lee-Carter asumiendo  una distribución Binomial, por eso el link es el logit
#lc() identifica que el ajuste a aplicar es el de Lee-Carter

LCfit_ESP_old <- fit(link="logit",lc(), data = ESPData)

#Se obtienen las tasas de mortalidad estimadas por el modelo Lee-Carter mediante la funcion "fitted",
#cuyo argumento es el objeto que contiene el ajuste del modelo.
#rates: qx

mxtHat_ESP_old <- fitted(LCfit_ESP_old, type = "rates")

#Se utiliza un paseo aleatorio bivariante con drift para proyectar las tasas de mortalidad en el horizonte parametrizado (12)
#La función forecast, cuando el argumento es un objeto de la clase "fitStMoMo" por defecto ajusta un paseo aleatorio con drif
#El argumento h indica el número de años que se quieren predecir

LCfor_ESP_old=forecast(LCfit_ESP_old,h=horizonte)

#Se obtinen las tasas de mortalidad proyectadas

mxtCentral_ESP_old <- LCfor_ESP_old$rates

# Bootstrap: cálculo de la incertidumbre
#---------------------------------------

#Se generan 1000 muestras aleatorios de una Binomial con probabilidades dadas por las que se obtienen al ajustar el modelo a los datos.
#Una vez generadas las muestras aleatorias, se ajusta un modelo Lee-Carter a cada una de ellas,
#obteniéndose los parámetros del modelo para cada una.

LCboot_ESP_old=bootstrap(LCfit_ESP_old,nBoot=1000,type="semiparametric")

#Con los parámetros obtenidos se utiliza un paseo aleatorio con drift para 
#proyectar las tasas en el horizonte considerado

LCsimPU_ESP_old=simulate(LCboot_ESP_old,h=horizonte)

#CALCULO DE LOS INTERVALOS DE CONFIANZA AL 99.5%
#Se calculan las curvas (de entre las simuladas anteriormente) tales que entre ellas estan 99.5% de las curvas

#Cálculo de los intervalos para la muestra

mxtHatPU0.05_ESP_old <- apply(LCsimPU_ESP_old$fitted, c(1, 2), quantile, probs = 0.005)
mxtHatPU99.5_ESP_old <- apply(LCsimPU_ESP_old$fitted, c(1, 2), quantile, probs = 0.995)

#Cálculo de los intervalos para las predicciones

mxtPredPU0.05_ESP_old <- apply(LCsimPU_ESP_old$rates, c(1, 2), quantile, probs = 0.005)
mxtPredPU99.5_ESP_old <- apply(LCsimPU_ESP_old$rates, c(1, 2), quantile, probs = 0.995)

