# Remueve todos los objetos creados
rm(list=ls())

eficiencia <- function(x){
  n = x
  tot = 0
  while(n>0){
    d = n %% 2
    n = n%/%2
    tot = tot + 2
    cat(d)
  }
  cat("\n")
  #cat(" ",x," ",tot," \n")
}

eficiencia(2)
eficiencia(10)
eficiencia(20)
eficiencia(30)
eficiencia(40)
eficiencia(50)
eficiencia(60)
eficiencia(73)