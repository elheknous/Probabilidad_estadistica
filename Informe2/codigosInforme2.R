
library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)
library(nortest)
library(gamlss)
library(gamlss.dist)
library(fitdistrplus)

######################################

no2 = base_datos$NO2
no2modificado = (base_datos %>% filter(NO2 < 142))$NO2

no2 = no2modificado
fw <-fitdist(no2,"weibull")
fln<-fitdist(no2,"lnorm")
fg <-fitdist(no2,"gamma")
fn <-fitdist(no2,"norm")
fe <-fitdist(no2,"exp")
# fb <-fitdist(no2,"beta") no se mueve entre 0 y 1
plot.legend<-c("Weibull","LogNormal","Gamma","Normal","Exponencial")

denscomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
qqcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
cdfcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)
ppcomp(list(fw,fln,fg,fn,fe), legendtext=plot.legend)


#chao exponeencial

gofstat(list(fg,fn,fln,fw,fe))
ks.test(no2,"pgamma",fg$estimate[1],fg$estimate[2])
ks.test(no2,"pweibull",fw$estimate[1],fw$estimate[2]) ##PARAMETROS


###########################################################

# X : Nivel de temperatura
temp = base_datos$TEMP
summary(temp)

#fw <-fitdist(temp,"weibull")
#fln<-fitdist(temp,"lnorm")
fn <-fitdist(temp,"norm")

plot.legend<-c("Normal")

denscomp(list(fn), legendtext=plot.legend)
qqcomp(list(fn), legendtext=plot.legend)
cdfcomp(list(fn), legendtext=plot.legend)
ppcomp(list(fn), legendtext=plot.legend)
ks.test(no2,"pnorm",fn$estimate[1],fn$estimate[2])

#no tiene distribucion continua estudiada






