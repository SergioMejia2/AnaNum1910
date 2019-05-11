# Remueve todos los objetos creados
rm(list=ls())

library(PolynomF)
require(deSolve)

euler2 = function(a,b,N, alp, bet){
  h = (b-a)/N
  
  t =c()
  u = c()
  y = c()
  
  t[1] = a
  y[1] = alp
  u[1] = bet
  
  it = 2
  while(it <= N+1)
  {
    t[it]=t[it-1]+h
    y[it]=y[it-1]+h*u[it-1]
    u[it]=u[it-1]+h*(-64*y[it-1])
    it = it + 1
  }
  return(y[N+1])
}

solucion = function(t){
  result = 2/3*cos(8*t)-1/6*sin(8*t)
  return (result)
}

biseccion = function(a,b,err, f) {
  # Grafico
  x = seq(a,b,0.1)
  Fx2 = f
  #plot(x,Fx(x),type="l",col="blue")
  #abline(h=0,v=0,col="red")
  
  inf = a
  sup = b
  aux = 0
  aux2 = 0
  it = 0
  result = Fx2(b)
  
  while(err < abs(result)){
    it = it + 1
    
    if((Fx2(inf)*Fx2(sup))<0){
      aux = (sup+inf)/2
      aux2 = Fx2(aux)
      
      if(abs(aux2) < err){
        break
      }else{
        if((Fx2(inf)*aux2)>0){
          inf =  aux
        }else{
          sup = aux
        }
      }
      
    }else{
      cat("Intervalo no valido\n")
      break
    }
  }

  #cat("Iteraciones =",it,"Resultado = ",aux,"\n")
  return(aux)
}

#POSICION EN T=3
res = euler2(0,3,1e5,2/3,-4/3)
val = solucion(3)

err = abs((res-val)/val) * 100
cat(res," ",val,"\n")
cat(err,"%\n")

#POSICION EN T=5
res = euler2(0,5,1e5,2/3,-4/3)
val = solucion(5)

err = abs((res-val)/val) * 100
cat(res," ",val,"\n")
cat(err,"%\n")

pts = 0
seqq = 0
i = pi/16
it = 1
while(it<=15)
{
  res = euler2(0,i,1e5,2/3,-4/3)
  pts[it] = res
  seqq[it] = i
  it = it + 1
  i = i+(pi/32)
}


plot(seqq,pts)
a = poly_calc(seqq,pts)
curve(a,add=T)
abline(h=0,v=0,col="red")
print(a)

#Primera derivada
pdiv = deriv(a)
#curve(pdiv,add=T,ylim=c(-10,10))
#abline(h=0,v=0,col="red")

#Segunda derivada
sdiv = deriv(pdiv)
#curve(sdiv,add=T,ylim=c(-50,50),xlim=c(0,pi/2))
#abline(h=0,v=0,col="red")

#Posicion de equilibrio
x = biseccion(0.4,0.8,10e-8,a)
cat("Posicion de equilibrio: ",x,"\n")
x = biseccion(0.7,1,10e-8,a)
cat("Posicion de equilibrio 2: ",x,"\n")
x = biseccion(1.3,1.4,10e-8,a)
cat("Posicion de equilibrio 3: ",x,"\n")

#Alcance maximo
#f'' < 0 max, f''>0 min
#como es un resprte el alcance maximo sera su minimo

puntocritico1 = biseccion(0,0.4,10e-8,pdiv)
puntocritico2 = biseccion(0.7,0.8,10e-8,pdiv)

pc1 = sdiv(puntocritico1)

pc2 = sdiv(puntocritico2)

if(pc1 > 0){
  cat("Alcance maximo: ",puntocritico1,"\n")
}else if(pc2 > 0){
  cat("Alcance maximo: ",puntocritico2,"\n")
}

####################
#Solucion exacta

fp = function(t,y, parms){
  s = -64*y
  return(list(s)) # ode requiere salida sea una lista
}
tis= seq(0,pi/2,by = 0.15707963267)

# Usamos la función ode()
sol = ode(c(180), tis, fp, parms=NULL, method = "rk4") # método Runge Kutta orden 4
# Salida
tabla = cbind(tis, sol[,2] )
colnames(tabla) = c("ti", " Ti ")
tabla
# Representación
plot(tis, sol[,2] )

it = 0
cat("solucion real ||","solucion rk4 ||","error","\n")
while (it <= 10){
  r = solucion(it)
  err = abs((r-sol[it+1,2])/sol[it+1,2]) * 100
  cat(r," || ",sol[it+1,2]," || ",err,"\n")
  
  it = it +1
}

plot(tis,sol[,2])
a = poly_calc(tis,sol[,2])
curve(a,add=T)
abline(h=0,v=0,col="red")
print(a)