




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

library(MortalitySmooth)
source("my.predict.R")
fitBIC3 <- Mort2Dsmooth(x=ages, y=years, Z=death ,offset=log(exposure),ndx=c(10,11))
newyears <- 1975:(2015+horizonte)
newdata <- list(x=ages, y=newyears) 
pre.for3 <- my.predict(fitBIC3, newdata=newdata, se.fit=TRUE)
#########################














