require(deSolve)


fp= function(t,y,parms){
  s= -64*y
  return (list(s))
}

tis = seq(0,10,1)

sol = ode(c(2/3),tis,fp,parms = NULL, method = "euler")

tabla = cbind(tis, sol[,2])
colnames(tabla) = c("ti", "Ti Euler")

tabla

plot(tis, sol[,2], main="Metodo de Euler")