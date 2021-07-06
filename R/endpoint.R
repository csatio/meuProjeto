source("funcoes.R")
library(plumber)

#* Retorna resultados modelo diamonds
#* @get /diamonds
diamonds <- function(){
  modelo_diamonds()
}

