# Remueve todos los objetos creados
rm(list=ls())

Fx <- function(x) ((3*sin(x)*cos(x))-(4*sin(x))+cos(x))
F1x <- function(x) ((-sin(x))-(4*cos(x))+(3*cos(2*x)))

newton <- function(a,b,err) {
  
  x0 = (a+b)/2;
  it = 0
  l <- 1
  
  repeat{
    
    correccion = Fx(x0)/F1x(x0)
    x1 = x0 - correccion
    dx = abs(correccion)
    x0 = x1
    it = it+1
    l[[it]] <- x1
    
    #cat(x1,"\n")

    if(dx <= err ) break;
  }
  
  # Grafico
  x = seq(-4,2,0.1)
  
  #Grafica cercana de la Solución
  plot(x,Fx(x),type="l",xlim=c(0.5,0.7),ylim=c(-0.5,0.5), col="blue",xlab="t",ylab="f(t)")
  par(new=TRUE)
  plot(l,Fx(l),type="p",xlim=c(0.5,0.7),ylim=c(-0.5,0.5),col="green",xlab="t",ylab="f(t)")
  title(main="Gráficas de f(t)")
  abline(h=0,v=0,col="red")
  
  #Grafica lejana de la Solución
  plot(x,Fx(x),type="l",xlim=c(-1,2),ylim=c(-3,3), col="blue",xlab="t",ylab="f(t)")
  par(new=TRUE)
  plot(l,Fx(l),type="p",xlim=c(-1,2),ylim=c(-3,3),col="green",xlab="t",ylab="f(t)")
  title(main="Gráficas de f(t)")
  abline(h=0,v=0,col="red")
  
  
  cat("Iteraciones = ", it, " Resultado = ", x1," t ", "\n")
}

newton(0,1,10e-8)
