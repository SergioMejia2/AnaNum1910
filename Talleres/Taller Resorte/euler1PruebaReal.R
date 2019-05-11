euler1 = function(init, xis ,func) {
  n = length(xis)
  h = xis[2] - xis[1]
  v.num = vector(length = n)
  v.num[1] = init
  for (j in 1:(n-1)) {
    v.num [j+1] = v.num [j] +
      h*func(xis[j], v.num[j]) }
  v.num}

# --- Pruebas
f = function(t,y) -64*y
xis= seq(0,10,1)
result1 = euler1(2/3, xis, f)

tabla = cbind(xis, result1)

colnames(tabla) = c("ti", "Ti Euler")

tabla

plot(xis, result1, main="Metodo de Euler")


solucion = function(t){
  result = 2/3*cos(8*t)-1/6*sin(8*t)
  return (result)
}

it = 0
cat("solucion real","\n")
while (it <= 10){
  r = solucion(it)
  
  cat(it,",", r,"\n")
  
  it = it +1
}