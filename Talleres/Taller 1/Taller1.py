from __future__ import print_function
import math


def metodo_horner(func,x_0):
    b_n = func[-1]
    num_sumas = 0
    num_multi = 0
    for i in reversed(range(-len(func),-1)):
        b_n = func[i] + b_n*x_0
        num_sumas = num_sumas + 1
        num_multi = num_multi + 1
    return (b_n, num_sumas, num_multi)


polinomios = [[-4,3,-3,0,2],[-4,3,0,-6,6,7],[0,-4,2,0,3,0,-5]]
x_0 = [-2,3,-1]

for i in range(len(polinomios)):
    (res, num_sumas, num_multi) = metodo_horner(polinomios[i],x_0[i])
    for a in range(len(polinomios[i])):
        linea = ""
        if polinomios[i][a] >= 0:
            linea = linea + "+"
        linea = linea + " " + str(polinomios[i][a]) + "x^" + str(a) + " "
        print(linea, end='')
    print("\nResultado:",res)
    print("Numero de sumas:",num_sumas)
    print("Numero de multiplicaciones:",num_multi)