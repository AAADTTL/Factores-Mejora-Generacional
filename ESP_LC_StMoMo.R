#install.packages('demography', repos='http://cran.us.r-project.org')



#PREPARAR DATOS PARA StmoMo



Dx_T_Esp <- read.delim("Dx_ESP.txt", header=F)
Exp_T_Esp <- read.delim("Ex_ESP.txt", header=F)
lx_T_Esp<- read.delim("lx_ESP.txt", header=F)

Dx_T_Esp2<-Dx_T_Esp[!is.na(Dx_T_Esp)]
Exp_T_Esp2<-Exp_T_Esp[!is.na(Exp_T_Esp)]
lx_T_Esp2<-lx_T_Esp[!is.na(lx_T_Esp)]


Dth<-matrix(Dx_T_Esp2,101,25)
Exp<-matrix(Exp_T_Esp2,101,25)
lx<-matrix(lx_T_Esp2,101,25)

Dth=Dth[41:90,]
Exp=Exp[41:90,]
lx=lx[41:90,]



Dx_T_Esp_1975 <- read.delim("Dx_ESP_1975_1990.txt", header=F)
Exp_T_Esp_1975 <- read.delim("Ex_ESP_1975_1990.txt", header=F)
lx_T_Esp_1975<- read.delim("lx_ESP_1975_1990.txt", header=F)

Dx_T_Esp2_1975<-Dx_T_Esp_1975[!is.na(Dx_T_Esp_1975)]
Exp_T_Esp2_1975<-Exp_T_Esp_1975[!is.na(Exp_T_Esp_1975)]
lx_T_Esp2_1975<-lx_T_Esp_1975[!is.na(lx_T_Esp_1975)]


Dth_1975<-matrix(Dx_T_Esp2_1975,91,16)
Exp_1975<-matrix(Exp_T_Esp2_1975,91,16)
lx_1975<-matrix(lx_T_Esp2_1975,91,16)

Dth_1975=Dth_1975[41:90,1:16]
Exp_1975=Exp_1975[41:90,1:16]
lx_1975=lx_1975[41:90,1:16]

Dth=cbind(Dth_1975,Dth)

Exp=cbind(Exp_1975,Exp)

lx=cbind(lx_1975,lx)





death=Dth
exposure=Exp
Age <- 40:89
Year <- 1975:2015


ages <-40:89
years <-1975:2015



ESPData=EWMaleData
ESPData$years=years
ESPData$ages=ages
ESPData$Dxt=Dth
ESPData$Ext=lx
ESPData$series="total"
ESPData$label="Spain"


#AJUSTE DEL MODELO

LCfit_ESP <- fit(link="logit",lc(), data = ESPData)

LCfitted_ESP=fitted(LCfit_ESP)

mxt_ESP <- LCfit_ESP$Dxt / LCfit_ESP$Ext

mxtHat_ESP <- fitted(LCfit_ESP, type = "rates")

# INFERENCIA INCLUYENDO  PARAMETER UNCERTAINTY

LCboot_ESP=bootstrap(LCfit_ESP,nBoot=1000,type="semiparametric")

LCsimPU_ESP=simulate(LCboot_ESP,h=horizonte)

#SIN PARAMETER UNCERTAINTY

LCfor_ESP=forecast(LCfit_ESP,h=horizonte)
LCsim_ESP=simulate(LCfit_ESP,nsim=1000, h=horizonte)

mxtCentral_ESP <- LCfor_ESP$rates

#95% Prediction intervals without parameter uncertainty (in sample and predictions)

mxtPred0.05_ESP <- apply(LCsim_ESP$rates, c(1, 2), quantile, probs = 0.005)
mxtPred99.5_ESP <- apply(LCsim_ESP$rates, c(1, 2), quantile, probs = 0.995)

#95% intervals with parameter uncertainty (in sample, and predictions)
mxtHatPU0.05_ESP <- apply(LCsimPU_ESP$fitted, c(1, 2), quantile, probs = 0.005)
mxtHatPU99.5_ESP <- apply(LCsimPU_ESP$fitted, c(1, 2), quantile, probs = 0.995)
mxtPredPU0.05_ESP <- apply(LCsimPU_ESP$rates, c(1, 2), quantile, probs = 0.005)
mxtPredPU99.5_ESP <- apply(LCsimPU_ESP$rates, c(1, 2), quantile, probs = 0.995)





