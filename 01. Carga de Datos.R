
# Ruta de los datos de entrada
setwd("K:/Clientes/Proyectos/Zurich/2017/2. Tablas de Mortalidad/3.Work/6. Implementación/Factores de mejora/DEFINITIVO/Datos")

#Lectura de los datos correspondientes a los años de 1991 a 2015
Dx_T_Esp <- read.delim("Dx_ESP.txt", header=F)
Exp_T_Esp <- read.delim("Ex_ESP.txt", header=F)
lx_T_Esp<- read.delim("lx_ESP.txt", header=F)

# Elimina registros vacios
Dx_T_Esp2<-Dx_T_Esp[!is.na(Dx_T_Esp)]
Exp_T_Esp2<-Exp_T_Esp[!is.na(Exp_T_Esp)]
lx_T_Esp2<-lx_T_Esp[!is.na(lx_T_Esp)]

# Almacenamiento de las defunciones, población estacionaria y supervivientes
Dth<-matrix(Dx_T_Esp2,101,25)
Exp<-matrix(Exp_T_Esp2,101,25)
lx<-matrix(lx_T_Esp2,101,25)

#Se restringe la edad desde los 40 hasta los 90 años, ya que los tres modelos solo se aplican
#simultáneamente al rango de edad 40-90
Dth=Dth[41:90,] #La primera edad es 0, por lo que el registro 41 corresponde a la edad 40.
Exp=Exp[41:90,]
lx=lx[41:90,]

#Se restringe la edad desde los 40 hasta los 95 años
Dth_total=matrix(Dx_T_Esp2,101,25)[41:(edad_max_modelo+1),]
Exp_total=matrix(Exp_T_Esp2,101,25)[41:(edad_max_modelo+1),]
lx_total=matrix(lx_T_Esp2,101,25)[41:(edad_max_modelo+1),]





#Lectura de los datos correspondientes a los años de 1975 a 1990
Dx_T_Esp_1975 <- read.delim("Dx_ESP_1975_1990.txt", header=F)
Exp_T_Esp_1975 <- read.delim("Ex_ESP_1975_1990.txt", header=F)
lx_T_Esp_1975<- read.delim("lx_ESP_1975_1990.txt", header=F)

# Elimina registros vacios
Dx_T_Esp2_1975<-Dx_T_Esp_1975[!is.na(Dx_T_Esp_1975)]
Exp_T_Esp2_1975<-Exp_T_Esp_1975[!is.na(Exp_T_Esp_1975)]
lx_T_Esp2_1975<-lx_T_Esp_1975[!is.na(lx_T_Esp_1975)]

# Almacenamiento de las defunciones, población estacionaria y supervivientes
Dth_1975<-matrix(Dx_T_Esp2_1975,91,16)
Exp_1975<-matrix(Exp_T_Esp2_1975,91,16)
lx_1975<-matrix(lx_T_Esp2_1975,91,16)

#Se restringe la edad desde los 40 hasta los 90 años, ya que los tres modelos solo se aplican
#simultáneamente al rango de edad 40-90
Dth_1975=Dth_1975[41:90,] #La primera edad es 0, por lo que el registro 41 corresponde a la edad 40.
Exp_1975=Exp_1975[41:90,]
lx_1975=lx_1975[41:90,]





# Unificación de las BBDD para las edades de 40 a 90 años
Dth=cbind(Dth_1975,Dth)
Exp=cbind(Exp_1975,Exp)
lx=cbind(lx_1975,lx)
