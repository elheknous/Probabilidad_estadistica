install.packages("readxl")
install.packages("tidyverse")
library(readxl)
str(Orders_East) #ver tipo de variables
Orders <- as.numeric(Orders_East$Ventas)
library(tidyverse)

Orders_East %>%
  group_by(State) %>% 
  tally()

base1 = Orders_East %>% 
  group_by(Segment) %>% 
  summarise(total = n(),
            media_ventas= mean(Ventas), #la media
            venta_min = min(Ventas))# minimo


