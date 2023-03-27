names(pokemon)
ataquesPokemon <- pokemon$attack

tablaFrecuencia <- as.data.frame(table(ataquesPokemon))
tipi <- transform(tablaFrecuencia, 
          freAcumulada = cumsum(tablaFrecuencia$Freq),
          frecRelativa = round(prop.table(tablaFrecuencia$Freq),3),
          frecRelativaAcumulada = round(cumsum(prop.table(tablaFrecuencia$Freq)),3))

