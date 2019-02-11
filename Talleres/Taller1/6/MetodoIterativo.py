import math

def f(x):
  return math.e**x - math.pi*x

def metodo(a, b, err):
    d = (b-a)/10
    x = a
    it = 0
    while d > err:
        x_i = x + d
        if f(x_i)*f(x) < 0:
            x_i = x_i - d
            d = d/10
        x = x_i
        it = it + 1
    print(it)
    return x

print(metodo(0,1,1e-8))
