# Remueve todos los objetos creados
rm(list=ls())
Original <-function(t) (2+cos(3*t))
Original2 <-function(t) (2-(exp(1)^t))

Fx <- function(x) (cos(3*x)+(exp(1)^x))

biseccion = function(a,b,err) {

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
      #cat(aux,"\n")
    }else{
      cat("Intervalo no valido\n")
      break
    }
  }
  
  # Grafico
  x = seq(-4,2,0.1)
  
  #Grafica lejana a la Solución
  plot(l,Original(l),type="p",xlim=c(-1.5,0.5),ylim=c(-1,3.5),col="orange",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(l,Original2(l),type="p",xlim=c(-1.5,0.5),ylim=c(-1,3.5),col="green",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Original(x),type="l",xlim=c(-1.5,0.5),ylim=c(-1,3.5), col="red",xlab="t",ylab="r(t)")
  par(new=TRUE)
  plot(x,Original2(x),type="l", xlim=c(-1.5,0.5),ylim=c(-1,3.5), col="blue",xlab="t",ylab="r(t)")
  title(main="Gráficas de las Funciones")
  
  cat("Iteraciones=",it,"t=",aux," rad ","r(t) = ",Original(aux),"\n")
}

biseccion(-1,1,10e-8)


