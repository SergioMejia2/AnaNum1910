rm(list=ls())
options(digits = 5)
require(deSolve)
require(PolynomF)
require(plotly)

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