#!/usr/bin/env python3
#Código realizado por Sergio Andrés Mejía Tovar
#Clase Análisis Numérico 2 - 1910
#Se recomienda correr en Python3
import math

def f(x):
    return math.e**x - x*math.pi

def f_prima(x):
    return math.e**x - math.pi

def metodoNewton(x_0, err):
    it = 0
    res = x_0
    f_x = f(x_0)
    while abs(f_x) > err :
        res = res - f(res) / f_prima(res)
        f_x = f(res)
        it = it + 1
    return (res, it)

x = 1
errores = [1e-8, 1e-5, 1e-4]
for error in errores:
    (res, it) = metodoNewton(x, error)
    print("Error:", error)
    print ("Raiz es:", res, "con", it, "iteraciones")
