from numpy import *

def gauss(a,b):
    n=len(b)
    for i in range(n):
        a[i] = a[i]+[b[i]]
    for e in range(n):
        t=a[e][e]
        for j in range(e,n+1):
            a[e][j]=a[e][j]/t
        for i in range(e+1,n):
            t=a[i][e]
            for j in range(e,n+1):
                a[i][j]=a[i][j]-t*a[e][j]
    x=[]
    for i in range(n):
        x=x+[0]
    x[n-1]=a[n-1][n]
    for i in range(n-1,-1,-1):
        s=0
        for j in range(i+1,n):
            s=s+a[i][j]*x[j]
        x[i]=a[i][n]-s
    return  x

A=[[2.6,0.3,2.4,6.2],[7.7,0.4,4.7,1.4],[5.1,9.9,9.5,1.5],[6.0,7.0,8.5,4.8]]
B=[50.78,47.36,91.48,98.17]

c=[[2.6,0.3,2.4,6.2],[7.7,0.4,4.7,1.4],[5.1,9.9,9.5,1.5],[6.1,7.0,8.5,4.8]]
d=[50.78,47.36,91.48,98.17]

Ea = linalg.norm(A)
Abarra = []
for i in range(4):
    u = []
    for j in range(4):
        u.append(c[i][j] - A[i][j])
        print (c[i][j])
    Abarra.append(u)

Xp = gauss(c,d)
print("\n",Xp)
X = gauss(A,B)
print("\n",X)

Ex = [Xp - X for Xp, X in zip(Xp, X)] #error de la solucion

#Error relativo de la solucion
Exn = linalg.norm(Ex,inf)
Xpn = linalg.norm(Xp,inf)
ErSol = Exn/Xpn
print("\n","Error de la solucion: ",ErSol,"->",ErSol*100,"%")

#Error relativo de la matriz

EAbarra = linalg.norm(Abarra, inf)
ErRel = EAbarra/Ea
print("\n","Error relativo de la matriz: ",ErRel,"->",ErRel*100,"%")
#n=len(a)
'''for i in  range (4):
    #s = len(a[i])
    print(len(c[i]),"\n")
    for j in range (4):
        print(A[i-1][j-1],end=" ")
        #Ea[i][j]= (c[i][j] - a[i][j])'''
