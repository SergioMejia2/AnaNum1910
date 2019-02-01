#Codigo hecho por Sergio Andrés Mejía Tovar
#Análisis Numérico Clase  2 - 1910
import math


def g(x):
    return math.e**x / math.pi


def g_inv(x):
    return math.log(x*math.pi)


def punto_fijo(a, b, x0, err):
    it = 1
    x_i = x0
    g_xi = g(x_i)
    ascend = False
    while abs(x_i - g_xi) > err:
        #print x_i, g_xi
        if x_i < a:
            ascend = True
        if ascend is False:
            x_i = g_xi
            g_xi = g(x_i)
        else:
            g_xi = x_i
            x_i = g_inv(g_xi)
        it = it + 1
    return (x_i, it)


# MAIN
v_a = 0
v_b = 1
print g(v_b)
if g(v_a) < v_a or g(v_b) > v_b:
    print("Error. Cambie el intervalo")
    exit()

x0 = (v_a+v_b)/2.0
(res, ite) = punto_fijo(v_a, v_b, x0, 10e-8)
print ("El resultado es", res, "con", ite, "iteraciones")