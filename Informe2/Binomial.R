
library(dplyr)   # manipulacion de datos
library(ggplot2) # visualizacion
library(readxl) # leer excel y escribir excel
library(moments) # asimetria y curtosis
library(tidyverse)
library(nortest)
library(gamlss)
library(gamlss.dist)
library(fitdistrplus)


#x = binomial(n = total;p = probabilidad) q = 0.85
#dbinom(x,n,p)
p = 0.01
n = 30

#p(x = 5)
dbinom(5,n,p)

#A lo mas 8
# p(x <= 8)
pbinom(8,n,p)

#p(x<2) = p(x = 0) + p(x = 1) 
dbinom(0,n,p) + dbinom(1,n,p)

#p(x >3) = 1 - p(x<=3)
1 - pbinom(3,n,p)

x = c(0:n)
mean(x)
y = dbinom(x,n,p)

plot(x,y)
table(x)

barplot(table(x,y))
 ####################################################################
##################################################################
###############################################################

base_datos = base_datos %>% 
  mutate(Temperatura_alta = ifelse(TEMP >= 25,1,0))
length(temp)
probs = NULL
n1 = round(length(base_datos$TEMP)*0.2,0)
for (i in 1:100) {
  sample.aux = temp %>% sample(n1,replace=F)
  probs = c(probs,mean(sample.aux))
  
}
p = mean(probs)
p










