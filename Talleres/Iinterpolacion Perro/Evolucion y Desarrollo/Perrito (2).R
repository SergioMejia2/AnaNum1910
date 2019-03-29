require(PolynomF)
require(graphics)
library(graphicsQC)

y=c(3,3.7,3.9,6  ,7.12,6.4 ,4.45,7 ,6.1 ,5.2 ,5.15,4.1 )                                                                                                       
x=c(1,2  ,5  ,7.3,10  ,13,17  ,20,23.5,24.6,26.5,27.5) 

y1=y[1:7]                                                                                                       
x1=x[1:7]
y2 = y[7:12]
x2 = x[7:12]
n <- length(x)
plot(x, y, pch=20, main = paste("Interpolacion del perrito con", n, "puntos"),xlim=c(0,31),ylim=c(0,9))
lines(spline(x1, y1, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x2, y2, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
