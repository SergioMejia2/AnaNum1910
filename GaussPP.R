# Remueve todos los objetos creados
rm(list=ls())

gaussPP = function(A, b){
  
  if(is.matrix(A)) {
    n = nrow(A); m = ncol(A)
    if (m != n) stop("'A' debe ser una matriz cuadrada.")
  }
  
  entrada = n*m+length(b)
  cat(entrada," ")
  op = 0
  
  # matriz ampliada
  Ab = cbind(A,b)
  
  # Eliminación
  for (k in 1:(n-1)){ # desde columna k=1 hasta k=n-1
    
    # índice del pivote máximo, en valor absoluto
    # wich.max( A[k:n,k] ) retorna índice del vector A[k:n,k] = (a_kk, a_(k+1)k,...,a_nk)
    # Como a_kk tendría índice 1, hay que corregir el índice sumando k-1.
    fila = which.max( abs(A[k:n,k]) ) + k-1
    op = op +2
    Ab[c(k, fila), ] = Ab[c(fila, k), ]
    
    # Si pivote es cero, det A = 0!
    if(A[fila,k]==0) stop("La matriz es singular")
    
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
      op = op + 3*(n-i)
      }
  }
  
  # Sustitución hacia atrás-------------------------
  # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn
  op = op + n
  
  for(i in (n-1):1 ){ # for
    x[i]= (Ab[i, n+1] -sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
    op = op +3
  }
  #cat(x,"\n")
  cat(op,"\n")
  return(x)
}

#--- Pruebas
A = matrix(c( 0, 2, 3, 3,
              -5, -4, 1, 4,
              0, 0, 0, 3,
              -4, -7, -8, 9), nrow=4, byrow=TRUE)
b = c(1,0,0,0)
################
C = matrix(c( exp(1), pi,
              1/3, -1/5), nrow=2, byrow=TRUE)
d = c(1/2,4)
################
E = matrix(c( 7, 1/2, 3,
              7, 4, 2,
              1, 1/4, 1), nrow=3, byrow=TRUE)
f = c(4,6,0)
##

cat(gaussPP(A,b), "\n")
cat(solve(A,b), "\n") 

cat("\n")

cat(gaussPP(C,d), "\n")
cat(solve(C,d), "\n")

cat("\n")

cat(gaussPP(E,f), "\n") 
cat(solve(E,f), "\n")
