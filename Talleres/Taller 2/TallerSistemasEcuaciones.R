#Taller Sistemas de Ecuaciones
#Sergio Andres Mejia Tovar
#Julian David Parada Galvis
#Analisis Numerico 1910 

library(pracma)
library(Matrix)

#PUNTO 1
n=4

D1<-eye(n, m = n)
D2<-ones(n, m = n)
D3<-zeros(n, m = n)

print("D1")
print(D1)
print("D2")
print(D2)
print("D3")
print(D3)


A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=n, byrow=TRUE)
print("A")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=n, byrow=TRUE)
print("b")
print(b)

#FUNCION REALIZADA PARA EL PUNTO 3 PERO UTILIZAD ACA TAMBIEN
diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

#T = -D^-1(L + U)
D = diag1(A)
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)

T = (-solve(D))%*%(L+U)
print("T")
print(T)
print("Norma")
print(norm(T,"F"))


#PUNTO 2
#b
print("Gauss-Seidel:")
tol = 1e-9
sol = itersolve(A, b, x0=c(1,2,1,1), tol=1e-9 , method = "Gauss-Seidel")
print(sol)

#c

jacobiPr <- function(A,b, x0, tol){
  x_k = matrix(x0)
  
  it = 0
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    it = it + 1
    if(it == tol)
      break
  }
  cat("SoluciÃ³n a 5 iteraciones: ",x_k,"\n")
}

x0 = c(1,2,1,1)
jacobiPr(A, b, x0, 5)

#PUNTO 3
poli = charpoly(A, info = FALSE)

#PUNTO 33

#a)
#Funcion modificada para eliminar la diagonal siempre
tril1 <- function(M, k = 0) {
  if (k == 0) {
    M[upper.tri(M, diag = TRUE)] <- 0
  } else {
    M[col(M) >= row(M) + k + 1] <- 0
    M[col(M)==row(M)] <- 0
    
  }
  return(M)
}

M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
print(M)
print(tril1(M, k=1))

#b)
#Funcion para sacar una matriz diagonal dada una matriz
diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

M = matrix(c(2,3,4,1,2,3,5,6,7),nrow=3)
print(M)
print(diag1(M))

#Punto 4
#Cree una funcion que cuente el numero de multiplicaciones 
#en el metodo directo de Gauss Jordan, para resolver un sistema 
#de nn ecuaciones y pruebelo para n=5

numMult = function(A, b){ # Se supone det(A) != 0
  mult = 0
  n = nrow(A) # = ncol(A) para que sea cuadrada
  
  # matriz ampliada
  Ab = cbind(A,b)
  print(Ab)
  # Eliminación
  for (k in 1:(n-1)){ # desde columna k=1 hasta k=n-1
    if(Ab[k,k]==0){ # intercambio de fila
      fila = which(Ab[k, ]!=0)[1]
      Ab[c(k, fila), ] = Ab[c(fila, k), ]
    }
    
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      mult = mult + 2*(ncol(Ab))
    }
  }
  
  # Sustitución hacia atrás-------------------------
  # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn
  mult = mult + n+1
  
  for(i in (n-1):1 ){
    x[i]= (Ab[i, n+1] - sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    mult = mult + 2*(n-2)
  }
  #cat(x, "\n")
  cat("Numero de multiplicaciones:", mult, "\n")
  return(x)
}

A = matrix(c( 0, 2, 3, 3, 3,
              -5, -4, 1, 4, 5,
              0, 0, 0, 3, 7,
              -4, -7, -8, 9,7,
              3, 4, 5, 5, 6), nrow=5, byrow=TRUE)
b = matrix(c(1,0,0,0,1), nrow=5, byrow=TRUE)
cat("Punto 4: ","\n")
numMult(A,b)

#Punto 5

#Punto a
#Se llega a los valores de alpha y beta por las operaciones de +
# alpha > 1+1 
# beta + 1 < 2
# de acuerdo a su posiscion en  la matrix
beta = 0
alpha = 3

A = matrix(c(2, 0, 1,
             beta,2 , -1,
             -1, 1, alpha), nrow=3, byrow=TRUE)
B = matrix (c(1,2,1),nrow=3, byrow=TRUE)
Ab = cbind(A,B)

print(Ab)

#puntp b y c
library("plot3D")

x = 0
y = 0
z = 0

diag1 <- function(M) {
  
  M[col(M)!=row(M)] <- 0
  
  return(M)
}

jacobiPr2 <- function(A,b, x0, tol){
  x_k = matrix(x0)
  
  D = diag1(A)
  L = tril(A,k=-1,diag = FALSE)
  U = triu(A,k=1,diag = FALSE)
  
  it = 1
  repeat
  {
    inn = matrix(b-((L+U)%*%x_k))
    D1 = (solve(D))
    xk1 = D1%*%inn
    cat("Error ",it," ",norm(xk1-x_k,"F")/norm(x_k),"\n")
    x_k = xk1
    
    x[[it]] = x_k[1]
    y[[it]] = x_k[2]
    z[[it]] = x_k[3]
    cat("Solucion iteracion ",it,": ",x[[it]]," ",y[[it]]," ",z[[it]],"\n")
    it = it + 1
    
    if(it == tol)
      break
  }
  lines3D(x, y, z, colvar = z, col = NULL, add = FALSE, theta = 20, phi = 20)
  cat("Solucion a ", tol ," iteraciones: ",x_k,"\n")
}

x1 = c(1,2,3)
jacobiPr2(A, B, x1, 10)

#Punto 6
A = matrix(c(-8.1, -7, 6.123, -2,
             -1, 4,-3, -1,
             0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
print("A")
print(A)
b = matrix(c(1.45,3,5.12,-4), nrow=4, byrow=TRUE)
print("b")
print(b)

Ab = cbind(A,b)
print(Ab)

#matrices diagonales
L = tril(A,k=-1,diag = FALSE)
U = triu(A,k=1,diag = FALSE)
print(L) 
print(U)

#factorizacion QR
gs <- gramSchmidt(A)
(Q <- gs$Q); (R <- gs$R)
print(Q)
print(R)
print(Q %*% R)  # = A

#Punto 7
#a
library(BB)
ecuaciones = function(x) {
  n = length(x)
  F = rep(NA, n)
  F[1] = x[1] - x[2]
  F[2] = x[1]^2 + x[2]^2 -1
  F
}
p0 = c(1,1) # n initial random starting guesses
sol = BBsolve(par=p0, fn=ecuaciones)
sol$par

plot(sol$par)
plot(ecuaciones)

#b
trigexp = function(x) {
  
  #se obtiene el numero de variables del sistema
  n = length(x)
  
  #se llena F con n elementos vacíos
  F = rep(NA, n)
  
  #Se ingresan las ecuaciones del sistema
  #Primera ecuación: F[1] = 3(x_1)^2 + 2(x_2) - 5 + sin( (x_1)-(x_2) )( sin( (x_1)+(x_2) ) )
  F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
  #tn1 es una secuencia de 2 hasta n-1
  tn1 = 2:(n-1)
  #Se ingresan |tn1| ecuaciones, es decir desde la ecuación 2 hasta la n-1
  F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
    ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                               x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
  #Se ingresa la ecuación n
  F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
  F
}
n = 10000
p0 = runif(n) # n initial random starting guesses
#se resuelve el sistema trigexp con BBsolve, utilizando n valores iniciales
sol = BBsolve(par=p0, fn=trigexp)
#Muestra por pantalla la solución del sistema para cada uno de los n valores iniciales
sol$par

#Punto 8

N <- 3
A <- Diag(rep(3,N)) + Diag(rep(-2, N-1), k=-1) + Diag(rep(-1, N-1), k=1)
x0 <- rep(0, N)
b = c(4,5,6)

itersolve(A, b, tol=1e-9 , method = "Gauss-Seidel")
