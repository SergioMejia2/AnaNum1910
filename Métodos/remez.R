# -------------------------------------------------------------------------------------
# ALGORITMO DE REMEZ PARA CALCULAR POLINOMIOS MINIMAX
# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Sergio Andrés Mejía Tovar   
# -------------------------------------------------------------------------------------
rm(list=ls())
library(PolynomF)
library(Deriv)
library(rootSolve)
library(ggplot2)

#Numeros de Chebyshev
numerosChebyShev = function(intervaloA,intervaloB,listaNumeros)
{
  numerosCalculados= c()
  
  for (i in listaNumeros) 
  {
    numerosCalculados= c(numerosCalculados, 1/2 * (intervaloA + intervaloB)
                         + 1/2 * (intervaloB-intervaloA) 
                         * cos((2*i-1) / (2 * length(listaNumeros))* pi) )
  }
  return (numerosCalculados)
}

f = function(x)
{
  sin(x)
}
n = 3
a = pi/64
b = -pi/64
E = 10e-5
alpha = 1.05

#PASO 1:
#Hallar n+2 nodos de Chebyshev en el intervalo [a,b] y sus imágenes f(x).
#Por la naturaleza del algoritmo se envian los intervalos de forma invertida
#para que los resultados estén ordenados
listaCalculada= numerosChebyShev(b,a,seq(1,n+2))
fxCheby = f(listaCalculada)

ns = 1
iteraciones = 1
while (ns) #El ciclo se romperá cuando Emax <= alpha*Emin
{
  #PASO 2:
  #Se crea un sistema matricial de ecuaciones lineales para hallar los coeficientes
  #del polinomio minimax interpolado.
  matrizDeEcuaciones = matrix(nrow = n+2,ncol = n+2)
  
  for (i in 1:(n+2)) 
  {
    for (j in 1:(n+1)) 
    {
      matrizDeEcuaciones[i,j] = listaCalculada[i]^(j-1)
    }
    matrizDeEcuaciones[i,n+2] = (-1)^(i-1)*E
  }
  
  print(matrizDeEcuaciones)
  
  #Se utiliza el método solve() de R para hallar los valores de los coeficientes
  
  minimax =  solve(matrizDeEcuaciones,fxCheby[1:(n+2)])

  #El polinomio interpolado estárá guardado en minimF utilizando la función
  #polynom() de la librería PolynomF.
  #Cabe resaltar que la última posición del arreglo solución no es parte de los
  #coeficientes del polinomio sino un error E, por lo que debe ser ignorado
  minimF = polynom(a=minimax[1:(length(minimax)-1)])
  
  err = function(x)
  {
    return((f(x)-minimF(x)))
  }
  relerr = function(x)
  {
    fx = abs((f(x)-minimF(x))/f(x))
    fx[is.na(fx)] <- 0
    return(fx)
  }
  
  #Se imprimen las gráficas originales, interpoladas
  pdf(paste("outIteration",iteraciones,".pdf"))
  print(ggplot(data.frame(x=c(a, b)), aes(x)) + stat_function(fun=f, color = "blue") + labs(title="f(x)") )
  print(ggplot(data.frame(x=c(a, b)), aes(x)) + stat_function(fun = minimF, color = "red") + labs(title=paste("minimax",minimF)) )
  print(ggplot(data.frame(x=c(a, b)), aes(x)) + stat_function(fun = minimF, color = "red") + stat_function(fun=f, color = "blue") + labs(title="Function+Minimax") )
  
  print(ggplot(data.frame(x=c(a, b)), aes(x)) + stat_function(fun=err, color = "blue") + labs(title="Absolute Error Function", x = "x", y = "Error"))
  
  print(ggplot(data.frame(x=c(a, b)), aes(x))  + stat_function(fun=relerr, color = "blue") + labs(title="Relative Error Function", x = "x", y = "Relative Error") )
  dev.off()
  
  fff = toString(minimF)
  
  #Se genera una función error, que es la diferencia de la función original y
  #el polinomio minimax. Esta función se deriva y se hallan las raíces de esta
  #derivada, que corresponderán a los extremos de la función error
  errf = function(x)
  {
    x1 = c()
    d = Deriv(f)  
    for(i in x)
      x1 = c(x1, d(i) - sum((seq(1,length(minimax)-2))*minimax[3:length(minimax)-1]*(i^(seq(0,length(minimax)-3)))))
    x = x1
    return(x)
  }
  extrema = uniroot.all(errf,c(a, b))

  #Se muestran los resultados:
  cat("Valores extremos de la funcion error: \n")
  print(extrema)
  
  cat("Polinomio minimax interpolado: \n")
  print(minimF)
  
  #Se calcula el error absoluto de los nuevos puntos con la función y se halla el
  #mayor y el menor de estos errores
  MaxError = max(abs(err(extrema)))
  MinError = min(abs(err(extrema)))
  cat("Error Maximo en los extremos: ", MaxError, "\n")
  cat("Error Minimo en los extremos: ", MinError, "\n")

  #El algoritmo se detendrá cuando estos errores mayor y menor sean iguales por
  #cierto porcentaje.
  if(MaxError < MinError*alpha)
  {
    cat("\nPolinomio Final: ", toString(minimF),"\n")
    break;
  }
  iteraciones = iteraciones + 1
  
  #En la siguiente iteración del algoritmo, los nuevos puntos de control para el
  #sistema de ecuaciones lineales serán los extremos del intervalo [a,b] y los valores
  #extremos de la función error
  listaCalculada = c(a,extrema,b)
  fxCheby = f(listaCalculada)
}
