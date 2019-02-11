# Remueve todos los objetos creados
rm(list=ls())

Original <-function(t) (2+cos(3*t))
Original2 <-function(t) (2-(exp(1)^t))

Fx <- function(x) (cos(3*x)+(exp(1)^x))
F1x <- function(x) ((-3*sin(3*x))+(exp(1)^x))

polar <- function (theta, r, color=4){
  y <- 0
  x <- 0
  ejex <- 1
  
  for (i in 1:length(r)){
    if(is.nan(r[i])== T){
      r[i] <- 0
    }
  }
  
  angulo <- seq(-max(theta),max(theta),by=theta[2]-theta[1])
  y <- r*sin(theta)
  x <- r*cos(theta)
  plot.new()
  plot.window(xlim = c(-max(r), max(r)), ylim = c(-max(r), max(r)), asp = 1)
  
  aux <- max(r)
  # Dibuja los ejes.
  while (aux > 0){
    fi <- aux*sin(angulo)
    cir <- aux*cos(angulo)
    points(cir,fi,pch="-",col="gray",cex=0.3)
    text(ejex+0.2,-0.2,ejex,col="gray")
    ejex <- ejex + 1
    aux <- aux - 1
  }
  
  abline(v=((max(cir)+min(cir))/2),col="gray")
  abline(h=((max(cir)+min(cir))/2),col="gray")
  segments(-max(r)+0.5,-max(r)+0.5,max(r)-0.5,max(r)-0.5,col="gray")
  segments(-max(r)+0.5,max(r)-0.5,max(r)-0.5,-max(r)+0.5,col="gray")
  
  points(x,y,pch=20,col=color,cex=1)
  
}

newton <- function(a,b,err) {
  #Grafica
  dim <- seq(0, 2*pi, by=pi/300) 
  r=2+cos(3*dim)
  r2=2-(exp(1)^dim)
  polar(dim,r,"blue")
  par(new=TRUE)
  polar(dim,r2,"green")
  title(main="Gráficas de las Funciones")
  
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

  #Grafica Cercana a la Solución
  #x = seq(-4,2,0.1)
  #plot(l,Original(l),type="p",xlim=c(-0.73,-0.68),ylim=c(1.45,1.55),col="orange",xlab="t",ylab="r(t)")
  #par(new=TRUE)
  #plot(l,Original2(l),type="p",xlim=c(-0.73,-0.68),ylim=c(1.45,1.55),col="green",xlab="t",ylab="r(t)")
  #par(new=TRUE)
  #plot(x,Original(x),type="l",xlim=c(-0.73,-0.68),ylim=c(1.45,1.55), col="red",xlab="t",ylab="r(t)")
  #par(new=TRUE)
  #plot(x,Original2(x),type="l", xlim=c(-0.73,-0.68),ylim=c(1.45,1.55), col="blue",xlab="t",ylab="r(t)")
  #title(main="Gráficas de la Solucion")
  
  #Grafica lejana a la Solución
  x = seq(-4,2,0.1)
  plot(l,Original(l),type="p",xlim=c(-3,0),ylim=c(-1,3.5),col="orange",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(l,Original2(l),type="p",xlim=c(-3,0),ylim=c(-1,3.5),col="green",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Original(x),type="l",xlim=c(-3,0),ylim=c(-1,3.5), col="red",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Original2(x),type="l", xlim=c(-3,0),ylim=c(-1,3.5), col="blue",xlab="t",ylab="r(t)")
  title(main="Gráficas de la Solucion")
  
  cat("Iteraciones = ", it, " t = ", x1," rad ","r(t) = ",Original(x1),"\n")
}

newton(-1,1,10e-8)