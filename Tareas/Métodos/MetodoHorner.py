#!/usr/bin/env python3
#Código realizado por Sergio Andrés Mejía Tovar
#Clase Análisis Numérico 2 - 1910
#Se recomienda correr en Python3
import math
import time
import random

random.seed(a=None)

current_milli_time = lambda: int(round(time.time() * 1000))

def metodo_horner(func, x_0):
    b_n = func[-1]
    for i in reversed(range (-len(func), -1)):
        b_n = func[i] + b_n*x_0
    return b_n

func = [2,3,4,5.6,math.e,math.pi,2,4,5,4,24,2,6272,2,242,26,26,2,2,67,0.2,math.e/4,0.2415,0.2326,0.23626,0.2377]
for a in range(0,900):
    func.append(random.random()/100000.0)
#    2+3x+4x²+5.6x³+ex⁴+pix⁵
x_0 = 2
current_milli_time = int(round(time.time() * 1000))
val = 0
for i in range(0, len(func)):
    val = val + func[i]*(x_0**i)
#print("\n",val)
current_milli_time2 = int(round(time.time() * 1000))
print(current_milli_time2-current_milli_time,end="")

current_milli_time = int(round(time.time() * 1000))
#print("\n",metodo_horner(func,x_0))
current_milli_time2 = int(round(time.time() * 1000))
print(current_milli_time2-current_milli_time)
