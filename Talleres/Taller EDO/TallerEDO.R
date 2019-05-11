rm(list=ls())
options(digits = 5)
require(deSolve)
require(PolynomF)

#Punto 1
fp= function(t,y,parms){
  dx = y[2]
  dz = 6*y[1]+y[2]
  return (list(c(dx,dz)))
}

solucionReal = function(x){
  r = 1/5*exp(1)^(-2*x)*(7+3*exp(1)^(5*x)) 
  return(r)
}

#h=0.1
tis = seq(0,2,by = 0.1)
sol = ode(c(2,-1),tis,fp,parms = NULL, method = "rk4")
tabla = cbind(tis, sol[,2])
tabla
plot(tis, sol[,2], main="Metodo de Runge-Kutta")


a = poly.calc(tis,sol[,2])
curve(a,add=T)
abline(h=0,v=0,col="red")
print(a)

rr=c()
it=0
i=1
while(i <= 21){
  rr[i] = solucionReal(it)
  i = i + 1
  it = it + 0.1
}
par(new=TRUE)
lines(tis, rr, col = "blue")


#h=0.2
tis2 = seq(0,4,by = 0.2)
sol2 = ode(c(2,-1),tis2,fp,parms = NULL, method = "rk4")
tabla2 = cbind(tis2, sol2[,2])
tabla2
plot(tis2, sol2[,2], main="Metodo de Runge-Kutta")

b = poly.calc(tis2,sol2[,2])
curve(b,add=T)
abline(h=0,v=0,col="red")
print(b)

rr=c()
it=0
i=1
while(i <= 21){
  rr[i] = solucionReal(it)
  i = i + 1
  it = it + 0.2
}
par(new=TRUE)
lines(tis2, rr, col = "green")

#Punto 2

fp= function(t,y,parms){
  dx = 3*y[1]-2*y[2]
  dz = 5*y[1]-4*y[2]
  return (list(c(dx,dz)))
}

tis = seq(0,1,by = 0.1)
sol = ode(c(3,6),tis,fp,parms = NULL, method = "euler")
tabla = cbind(tis, sol[,2],sol[,3])
print(tabla)

#plot(tis, sol[,2], main="Metodo de Euler Mejorado")

fx1 = function(x,y){
  r = 3*x-2*y
  return(r)
}

fy2 = function(x,y){
  rr = 5*x-4*y
  return(rr)
}

eulerpp = function(x0,y0,f1,f2){
  
  u=0
  v=0
  
  xr=0
  yr=0
  
  u[1]=x0
  v[1]=y0
  
  xr[1]=x0
  yr[1]=y0
  
  tis = seq(0,1,by = 0.1)
  
  i = 2
  cat("Xn", " ","Yn","\n")
  cat(xr[1], " ",yr[1],"\n")
  while(i <= 11){
    
    u[i]=xr[i-1]+0.1*(f1(xr[i-1],yr[i-1]))
    v[i]=yr[i-1]+0.1*(f2(xr[i-1],yr[i-1]))
    
    #cat(u[i], " ",v[i],"\n")
    
    xr[i] = xr[i-1] + (0.1/2)*(f1(xr[i-1],yr[i-1])+f1(u[i],v[i]))
    yr[i] = yr[i-1] + (0.1/2)*(f2(xr[i-1],yr[i-1])+f2(u[i],v[i]))
    
    i = i + 1
  }
  cat("Euler Mejorado","\n")
  for(i in 1:length(xr))
  {
    cat(i*0.1-0.1, ": ",xr[i]," ", yr[i], "\n")
  }
}

eulerpp(3,6,fx1,fy2)

#Punto 3
#y''-y'-x+y+1 = 0, y(0)=1, y'(0)=2
#Veinte puntos de la solución, gráfica e interpolación
fp= function(x,datos,parms){
  
  y = datos[1]; z = datos[2] 
  dy = z
  dz = z + x - y - 1
  return (list(c(dy,dz)))
}


xis = seq(0,2,by = 0.1)

sol = ode(c(1,2),xis,fp,parms = NULL, method = "rk4")

tabla = cbind(xis, sol[,2])

tabla

f <- function(x)
{
  fx = x + exp(1)^(x/2)*cos((sqrt(3)*x)/2) + (exp(1)^(x/2)*sin((sqrt(3)*x)/2))/sqrt(3)
  return(fx)
}


plot(xis, sol[,2], main="Metodo de Runge Kutta Grado 4")
b = poly.calc(xis,sol[,2])
curve(f, xlim=c(0,2), col="chartreuse4",add=T)

curve(b,add=T, col="blue")
abline(h=0,v=0,col="red")
print("Funcion de interpolacion: ")
print(b)

#Punto 4

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

fp= function(t,y,parms){
  dz = -4*y[2] + cos(2*t)
  dy = y[1]
  return (list(c(dz,dy)))
}

tis = seq(0,2*pi,by = 0.1*pi)
sol = ode(c(0,1),tis,fp,parms = NULL, method = "rk4")
tabla = cbind(tis, sol[,2],sol[,3])
print(tabla)
#plot(tis, sol[,3], main="Posicion del resorte")

a = poly.calc(tis[1:10],sol[,3][1:10])
print(a)
x = biseccion(0,2,10e-8,a)
cat("Posicion de equilibrio  en t =",x,"\n")

fpDespues= function(t,y,parms){
  dz = -4*y[2] 
  dy = y[1]
  return (list(c(dz,dy)))
}

tis2 = seq(2*pi,4*pi,by = 0.1*pi)
sol2 = ode(c(sol[,2][21],sol[,3][21]),tis2,fpDespues,parms = NULL, method = "rk4")
tabla2 = cbind(tis2, sol2[,2],sol2[,3])
print(tabla2)
aux = 0
tisAux = 0
for(i in 1:length(sol[,3])){
  aux[i]=sol[,3][i]
  tisAux[i]=tis[i]
}
for(i in 1:length(sol2[,3])){
  aux[i+length(sol[,3])]=sol2[,3][i]
  tisAux[i+length(sol[,3])]=tis2[i]
}

plot(tisAux, aux, main="Posicion del resorte")

abline(h=0,v=0,col="red")
lines(spline(tisAux, aux, n = 201))

solucionreal = function(t){
  rr = (1/4)*t*sin(2*t)+cos(2*t)
}
curve(solucionreal, add = T, col='chartreuse4', from = 0, to=2*pi)

solucionrealDespues = function(t){
  rr = 0.983573*cos(2*t)+3.14884*sin(t)*cos(t)
}
curve(solucionrealDespues, add = T, col='chartreuse4', from =2*pi, to=4*pi)

#Punto 5
#x' = x -3y
#y' = 3x+7y
#Veinte puntos de la solución, gráfica e interpolación
fp= function(t,datos,parms){
  
  x = datos[1]; y = datos[2] 
  dx = x - 3*y
  dy = 3*x + 7*y
  return (list(c(dx,dy)))
}


tis = seq(0,2,by = 0.1)

sol = ode(c(1,1),tis,fp,parms = NULL, method = "rk4")

tabla = cbind(tis, sol[,2])

tabla

f <- function(x)
{
  fx = exp(1)^(4*x)*(1-6*x)
  return(fx)
}


plot(tis, sol[,2], main="Metodo de Runge Kutta Grado 4")
b = poly.calc(tis,sol[,2])
curve(f, xlim=c(0,2), col="chartreuse4",add=T)

curve(b,add=T, col="blue")
abline(h=0,v=0,col="red")
print("Funcion de interpolacion: ")
print(b)

for(i in 1:length(sol[,2]))
{
  err = abs((sol[,2][i]-f(tis[i]))/(f(tis[i])))
  cat("x=",tis[i]," f(x)=",f(tis[i])," err=",err*100,"%\n")
}