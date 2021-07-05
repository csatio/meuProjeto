source("funcoes.R")
library(plumber)

#* Retorna a soma de dois números
#* @param a O primeiro número
#* @param b O segundo número
#* @post /diamonds
diamonds <- function(){
  modelo_diamonds()
}

