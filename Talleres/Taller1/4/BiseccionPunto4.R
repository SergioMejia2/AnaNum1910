# Remueve todos los objetos creados
rm(list=ls())

Original <-function(t) (2+cos(3*t))
Original2 <-function(t) (2-(exp(1)^t))

Fx <- function(x) (cos(3*x)+(exp(1)^x))

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

biseccion = function(a,b,err) {
  
  #Grafica
  dim <- seq(0, 2*pi, by=pi/300) 
  r=2+cos(3*dim)
  r2=2-(exp(1)^dim)
  polar(dim,r,"blue")
  par(new=TRUE)
  polar(dim,r2,"green")
  title(main="Gráficas de las Funciones")
  
  inf = a
  sup = b
  aux = 0
  aux2 = 0
  it = 0
  result = Fx(b)
  
  l <- 1
  
  while(err < abs(result)){
    it = it + 1
    
    if((Fx(inf)*Fx(sup))<0){
      aux = (sup+inf)/2
      aux2 = Fx(aux)
      
      if(abs(aux2) < err){
        break
      }else{
        if((Fx(inf)*aux2)>0){
          inf =  aux
        }else{
          sup = aux
        }
      }
      l[[it]] <- aux
    }else{
      cat("Intervalo no valido\n")
      break
    }
  }
  
  #Grafica de la Solución
  x = seq(-4,2,0.1)
  plot(l,Original(l),type="p",xlim=c(-1.5,0.5),ylim=c(-1,3.5),col="orange",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(l,Original2(l),type="p",xlim=c(-1.5,0.5),ylim=c(-1,3.5),col="green",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Original(x),type="l",xlim=c(-1.5,0.5),ylim=c(-1,3.5), col="red",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Original2(x),type="l", xlim=c(-1.5,0.5),ylim=c(-1,3.5), col="blue",xlab="t",ylab="r(t)")
  title(main="Gráficas de la Solucion")
  
  cat("Iteraciones=",it,"t=",aux," rad ","r(t) = ",Original(aux),"\n")
}

biseccion(-1,1,10e-8)