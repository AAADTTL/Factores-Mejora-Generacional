
# Carga de Librerías
#-------------------
library(StMoMo)
library(mgcv)
library(MortalitySmooth)
library(abind)

# Definición de los parámetros de entrada
#----------------------------------------

# Horizonte de proyección
horizonte=12
# Edad máxima sobre la que se ajusta el modelo
edad_max_modelo=95
# Edad máxima sobre la que se proyectan los factores de mejora generacionales
proyeccion_modelo=120


# Ruta Códigos
#-------------
setwd("K:/Clientes/Proyectos/Zurich/2017/2. Tablas de Mortalidad/3.Work/6. Implementación/Factores de mejora/DEFINITIVO/Codigos")


# Carga los datos de entrada
#---------------------------
source("01. Carga de Datos.R")

setwd("K:/Clientes/Proyectos/Zurich/2017/2. Tablas de Mortalidad/3.Work/6. Implementación/Factores de mejora/DEFINITIVO/Codigos")


# Ajuste modelo Lee-Carter (40 a 89 años)
#----------------------------------------
source("02. Ajuste LeeCarter 40 a 89.R")


# Ajuste modelo CBD (40 a 89 años)
#----------------------------------------
source("03. Ajuste CBD 40 a 89.R")


# Ajuste modelo PSpline (40 a 89 años)
#----------------------------------------
source("04. Ajuste Pspline 40 a 89.R")


# Ajuste modelo Lee-Carter (90 a 95 años)
#----------------------------------------
source("05. Ajuste LeeCarter 90 a 95.R")


# Cálculo de los factores de mejora
#----------------------------------
source("06. Calculo Factores Mejora.R")


# Suavizadode los factores de mejora y proyección de los mismos
#--------------------------------------------------------------
source("07. Suavizado Factores Mejora y Proyeccion.R")


# Guardar los resulatados
#------------------------
setwd("K:/Clientes/Proyectos/Zurich/2017/2. Tablas de Mortalidad/3.Work/6. Implementación/Factores de mejora/DEFINITIVO")

Factores_Mejora_Esp<-data.frame(cbind(40:proyeccion_modelo,cbind(FM_MEDIO_ESP_120,FM_0.995_ESP_120)))
names(Factores_Mejora_Esp)[1]<-"EDAD"

write.table(Factores_Mejora_Esp,"Fact_Mej_120.txt",row.names = FALSE, dec = ",")
