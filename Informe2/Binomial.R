
library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)
library(nortest)
library(gamlss)
library(gamlss.dist)
library(fitdistrplus)


 ####################################################################
##################################################################
###############################################################

base_datos = base_datos %>% 
  mutate(NO2_alto = ifelse(NO2 >= 80,1,0))
probs = NULL
n1 = round(length(base_datos$NO2)*0.2,0)
for (i in 1:1000) {
  sample.aux = base_datos$NO2_alto %>% sample(n1,replace=F)
  probs = c(probs,mean(sample.aux))
  
}
p = mean(probs)
p

#install.packages("mosaic")
library(mosaic)

pbinom(20,size = 40,prob = p) -pbinom(15,size = 40,prob = p)
plotDist(dist="binom",size = 40,prob =p)
plotDist(dist="binom",size = n,prob =p,kind ="histogram")










