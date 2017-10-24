library(StMoMo)
library(mgcv)
library(MortalitySmooth)
library(abind)

#setwd("K:/Clientes/Proyectos/Zurich/2017/2. Tablas de Mortalidad/2.Comunicación/Recibido/2017.10.04 - R - Implementación Factores Mejora UC3M/Codigo")
setwd("K:/Clientes/Proyectos/Zurich/2017/2. Tablas de Mortalidad/3.Work/4. Modelado/Factores de mejora/UC3M/CODIGOS Y BBDD")

horizonte=12
edad_max=99

#AJUSTE LEE-CARTER EDADES 40-89
source("ESP_LC_StMoMo.R")
#AJUSTE LEE_CARTER 90-95
source("ESP_LC_StMoMo_old.R")
#AJUSTE CBD
source("ESP_CBD_StMoMo.R")
#AJUSTE PSPLINE
source("ESP_Pspline.R")
#CALCULO FACTORES DE MEJORA
source("Factores_Mejora_FINAL.R")
matplot(40:edad_max,cbind(FM_MEDIO_ESP,FM_0.995_ESP),type="l",col=1,xlab="Edades", ylab="Factor de mejora en %",
main="Factores de mejora para cada edad",cex.main=0.9)
legend(56,4.5, legend=c("Best estimate","Estres 99.5%"), lty=c(1,2),col=c(1,1),bty="n",cex=0.7)



#SALVAR RESULTADOS
write.table(cbind(FM_MEDIO_ESP,FM_0.995_ESP),"Fact_Mej.txt")
