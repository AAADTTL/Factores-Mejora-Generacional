#FACTORES DE MEJORA
FM=function(A)
{m=ncol(A)
fm=(-1)*((A[,m]/A[,1])^(1/m)-1)
return(fm)
}


#Lee-Carter

library(abind)

B=abind(LCsimPU_ESP$rates,LCsimPU_ESP_old$rates[51:(edad_max-39),,],along=1)

A=apply(B, 3, FM)

A=apply(A,1,quantile, probs=0.995)

CC=rbind(mxtCentral_ESP,mxtCentral_ESP_old[51:(edad_max-39),])


FM_LC_ESP=100*cbind(FM(CC),A)


# CBD


A=apply(CBDsimPU_ESP$rates, 3, FM)
A=apply(A,1,quantile, probs=0.995)
CC=CBDmxtCentral_ESP

FM_CBD_ESP=100*cbind(FM(CC),A)

################P-SPLINE


B=exp(pre.for3$fit)
B=1-exp(-B)
A=FM(B[,43:53])*100
#A=FM(CC[,43:53])*100


CC=exp(pre.for3$fit-2.57*pre.for3$se.fit)
CC=1-exp(-CC)

FM_SP99.5_ESP=100*FM(CC[,43:53])
FM_SP_ESP=cbind(A,FM_SP99.5_ESP)




############## MEDIA


CC=(apply(cbind(FM_SP_ESP[,1], FM_LC_ESP[1:50,1],FM_CBD_ESP[,1]),1,mean))
FM_MEDIO_ESP=(c(CC,FM_LC_ESP[51:(edad_max-39),1]))
x=1:(edad_max-39)


library(mgcv)

FM_MEDIO_ESP=gam(FM_MEDIO_ESP~s(x,k=5))$fitted



CC=(apply(cbind(FM_SP_ESP[,2], FM_LC_ESP[1:50,2],FM_CBD_ESP[,2]),1,mean))
FM_0.995_ESP=(c(CC,FM_LC_ESP[51:(edad_max-39),2]))
x=1:(edad_max-39)

FM_0.995_ESP=gam(FM_0.995_ESP~s(x,k=5))$fitted



