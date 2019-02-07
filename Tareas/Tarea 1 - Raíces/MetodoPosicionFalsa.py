#!/usr/bin/env python3
#Código realizado por Sergio Andrés Mejía Tovar
#Clase Análisis Numérico 2 - 1910
#Se recomienda correr en Python3
import math

def f(x):
    return math.e**x - x*math.pi

def metodo_posicionf(a=0, b=2, err=10e-8):
    it = 0
    a_i = a
    b_i = b
    x_i = f(a_i)
    f_a = f(a_i)
    f_b = f(b_i)
    c_i = b_i - f(b_i)*((b_i-a_i)/(f(b_i)-f(a_i)))
    if f_a*f_b < 0:
        while abs(x_i) > err:
            f_a = f(a_i)
            f_b = f(b_i)
            c_i = b_i - f(b_i)*((b_i-a_i)/(f(b_i)-f(a_i)))
            x_c_i = f(c_i)
            if(abs(x_c_i) > err):
                if x_c_i*f_a < 0 :
                    b_i = c_i
                elif x_c_i*f_b < 0 :
                    a_i = c_i
            else:
                x_i = x_c_i
            it = it + 1
    return (c_i, it)

print("RAIZ 1")
a = 0
b = 1
errs = [10e-8, 10e-5, 10e-4]
for err in errs:
    print("ERROR:", err)
    (val, it) = metodo_posicionf(a, b, err)
    #it_t = math.log10((b-a)/err)/math.log10(2)
    #print("El valor teorico de iteraciones es i>", math.ceil(it_t))
    print("El valor es", val, "y las iteraciones son", it)

print("RAIZ 2")
a = 1
b = 2
for err in errs:
    print("ERROR:", err)
    (val, it) = metodo_posicionf(a, b, err)
    #it_t = math.log10((b-a)/err)/math.log10(2)
    #print("El valor teorico de iteraciones es i>", math.ceil(it_t))
    print("El valor es", val, "y las iteraciones son", it)
