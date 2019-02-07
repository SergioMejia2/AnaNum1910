#!/usr/bin/env python3
#Código realizado por Sergio Andrés Mejía Tovar
#Clase Análisis Numérico 2 - 1910
#Se recomienda correr en Python3
import math

def f(x):
    return math.e**x - x*math.pi

def metodo_secante(x_0,x_1,err):
    it = 0
    f_x = f(x_0)
    while abs(f_x) > err :
        x_2 = x_0 - f(x_0)*((x_1-x_0)/(f(x_1)-f(x_0)))
        x_0 = x_1
        x_1 = x_2
        f_x = f(x_2)
        it = it + 1
    return (x_2, it)

x_0 = [4,1]
x_1 = [3,0.6]
errores = [1e-8, 1e-5, 1e-4]
for x in range(0,2):
    for error in errores:
        (res, it) = metodo_secante(x_0[x], x_1[x], error)
        print("Error:", error)
        print ("Raiz es:", res, "con", it, "iteraciones")
