import math

def f(x):
    return x*(math.e**x) - math.pi

def biseccion(a=0, b=2, err=10e-8):
    it = 0
    a_i = a
    b_i = b
    x_i = f(a_i)
    f_a = f(a_i)
    f_b = f(b_i)
    c_i = (a_i+b_i)/2
    if f_a*f_b < 0:
        while abs(x_i) > err:
            f_a = f(a_i)
            f_b = f(b_i)
            c_i = (a_i+b_i)/2.0
            x_c_i = f(c_i)
            print(x_c_i)
            if(abs(x_c_i) > err):
                if x_c_i*f_a < 0 :
                    b_i = c_i
                elif x_c_i*f_b < 0 :
                    a_i = c_i
            else:
                x_i = x_c_i
            it = it + 1
    return (c_i, it)

a = 0
b = 2
err = 10e-8
(val,it) = biseccion(a,b,err)
it_t = math.log10((b-a)/err)/math.log10(2)
print("El valor teórico de iteraciones es i>", math.ceil(it_t))
print("El valor es", val, "y las iteraciones son", it)
