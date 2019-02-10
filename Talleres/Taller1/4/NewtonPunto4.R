# Remueve todos los objetos creados
rm(list=ls())

Original <-function(t) (2+cos(3*t))
Original2 <-function(t) (2-(exp(1)^t))

Fx <- function(x) (cos(3*x)+(exp(1)^x))
F1x <- function(x) ((-3*sin(3*x))+(exp(1)^x))

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

    if(dx <= err ) break;
  }
  
  # Grafico
  x = seq(-4,2,0.1)
  
  #Grafica Cercana a la Solución
  #plot(l,Original(l),type="p",xlim=c(-0.73,-0.68),ylim=c(1.45,1.55),col="orange",xlab="t",ylab="r(t)")
  #par(new=TRUE)
  #plot(l,Original2(l),type="p",xlim=c(-0.73,-0.68),ylim=c(1.45,1.55),col="green",xlab="t",ylab="r(t)")
  #par(new=TRUE)
  #plot(x,Original(x),type="l",xlim=c(-0.73,-0.68),ylim=c(1.45,1.55), col="red",xlab="t",ylab="r(t)")
  #par(new=TRUE)
  #plot(x,Original2(x),type="l", xlim=c(-0.73,-0.68),ylim=c(1.45,1.55), col="blue",xlab="t",ylab="r(t)")
  #title(main="Gráfica de la Solución")
  
  #Grafica lejana a la Solución
  plot(l,Original(l),type="p",xlim=c(-3,0),ylim=c(-1,3.5),col="orange",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(l,Original2(l),type="p",xlim=c(-3,0),ylim=c(-1,3.5),col="green",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Original(x),type="l",xlim=c(-3,0),ylim=c(-1,3.5), col="red",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Original2(x),type="l", xlim=c(-3,0),ylim=c(-1,3.5), col="blue",xlab="t",ylab="r(t)")
  title(main="Gráficas de las Funciones")
  
  cat("Iteraciones = ", it, " t = ", x1," rad ","r(t) = ",Original(x1),"\n")
}

newton(-1,1,10e-8)