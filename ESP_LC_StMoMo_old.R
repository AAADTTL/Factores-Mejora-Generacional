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

Dth=Dth[41:(edad_max+1),]
Exp=Exp[41:(edad_max+1),]
lx=lx[41:(edad_max+1),]



death=Dth
exposure=Exp
Age <- 40:edad_max
Year <- 1991:2015


ages <-40:edad_max
years <-1991:2015






ESPData=EWMaleData
ESPData$years=years
ESPData$ages=ages
ESPData$Dxt=Dth
ESPData$Ext=lx
ESPData$series="total"
ESPData$label="Spain"




LCfit_ESP_old <- fit(link="logit",lc(), data = ESPData)

LCfitted_ESP_old=fitted(LCfit_ESP_old)

#CON PARAMETER UNCERTAINTY

LCboot_ESP_old=bootstrap(LCfit_ESP_old,nBoot=1000,type="semiparametric")

LCsimPU_ESP_old=simulate(LCboot_ESP_old,h=horizonte)

#SIN PARAMETER UNCERTAINTY

LCfor_ESP_old=forecast(LCfit_ESP_old,h=horizonte)
LCsim_ESP_old=simulate(LCfit_ESP_old,nsim=1000, h=horizonte)

mxt_ESP_old <- LCfit_ESP_old$Dxt / LCfit_ESP_old$Ext
mxtHat_ESP_old <- fitted(LCfit_ESP_old, type = "rates")
mxtCentral_ESP_old <- LCfor_ESP_old$rates
#95% Prediction intervals without parameter uncertainty
mxtPred0.05_ESP_old <- apply(LCsim_ESP_old$rates, c(1, 2), quantile, probs = 0.005)
mxtPred99.5_ESP_old <- apply(LCsim_ESP_old$rates, c(1, 2), quantile, probs = 0.995)
#95% intervals with parameter uncertainty (in sample, and predictions)
mxtHatPU0.05_ESP_old <- apply(LCsimPU_ESP_old$fitted, c(1, 2), quantile, probs = 0.005)
mxtHatPU99.5_ESP_old <- apply(LCsimPU_ESP_old$fitted, c(1, 2), quantile, probs = 0.995)
mxtPredPU0.05_ESP_old <- apply(LCsimPU_ESP_old$rates, c(1, 2), quantile, probs = 0.005)
mxtPredPU99.5_ESP_old <- apply(LCsimPU_ESP_old$rates, c(1, 2), quantile, probs = 0.995)
#Plot
x <- c("93","95")
matplot(LCfit_ESP_old$years, t(mxt_ESP_old[x, ]),
xlim = range(LCfit_ESP_old$years, LCfor_ESP_old$years),
ylim = range(mxtHatPU99.5_ESP_old[x, ], mxtPredPU0.05_ESP_old[x, ], mxt_ESP_old[x, ]),
type = "p", xlab = "years", ylab = "mortaPU_lity rates (log scale)",
log = "y", pch = 20, col = "black")
matlines(LCfit_ESP_old$years, t(mxtHat_ESP_old[x, ]), lty = 1, col = "black")
matlines(LCfit_ESP_old$years, t(mxtHatPU0.05_ESP_old[x, ]), lty = 5, col = "red")
matlines(LCfit_ESP_old$years, t(mxtHatPU99.5_ESP_old[x, ]), lty = 5, col = "red")
matlines(LCfor_ESP_old$years, t(mxtCentral_ESP_old[x, ]), lty = 4, col = "black")
matlines(LCsim_ESP_old$years, t(mxtPred0.05_ESP_old[x, ]), lty = 3, col = "black")
matlines(LCsim_ESP_old$years, t(mxtPred99.5_ESP_old[x, ]), lty = 3, col = "black")
matlines(LCsimPU_ESP_old$years, t(mxtPredPU0.05_ESP_old[x, ]), lty = 5, col = "red")
matlines(LCsimPU_ESP_old$years, t(mxtPredPU99.5_ESP_old[x, ]), lty = 5, col = "red")
text(1986, mxtHatPU0.05_ESP_old[x, "1995"], labels = c("x=40", "x=60", "x=80"))









