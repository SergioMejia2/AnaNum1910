#!/usr/bin/env python3
#Código realizado por Sergio Andrés Mejía Tovar y Julián David Parada Galvis
#Clase Análisis Numérico 2 - 1910
#Se recomienda correr en Python3
import math

def raiz_n(n, val, x_0, err):
    x_k = x_0
    deltax_k = 1
    it = 0
    while abs(deltax_k) > err:
        deltax_k = ( ( val/(x_k**(n-1)) ) - x_k )/n
        x_k = x_k + deltax_k
        it = it + 1
    return (x_k, it)

n = 5
val = 789
x_0 = 9
err = 1e-8
print(raiz_n(n,val,x_0,err))

#Sea un raiz n-esima de un valor val, un error err y un valor inicial supuesto de x_0
#La raíz enésima será un método iterativo de la siguiente forma
#deltax_k = 1/n * (val/(x_k^(n-1)) - x_k)
#x_k+1 = x_k + deltax_k
#Se realiza hasta que abs(deltax_k) < err
#El intervalo de convergencia es [0,inf)
