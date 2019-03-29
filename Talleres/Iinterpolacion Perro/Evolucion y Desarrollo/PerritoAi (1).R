require(PolynomF)
require(graphics)
library(graphicsQC)
rm(list=ls())
x = c(0.45, 1.01 ,5.75, 7.46, 11.28, 18.46, 21.25,24.15, 24.83, 25.9 , 27.77, 30.16, 30.8, 30.81, 29.45, 27.3, 26.91, 26.59, 26.21, 25.78, 24.97, 20.32, 19.54, 18.8, 14.04, 12.34, 11.48, 9.55, 8.3  ,9.0,9.05,8.8 ,7.37,0.45)
y = c(2.29, 2.98 ,3.67, 5.41, 7.45 , 4.49 , 7.25  ,7.05 , 6.48 , 5.70 , 5.75 , 5.00 , 3.9  , 2.6  , 1.15 , 0.69, 0.55 , 0.59 , 0.44 , 0.52 , 0.44 , 1.01 , 0.8  , 1.08, 0.95 , 1.08 , 1.33 , 1.03, 1.64  ,2.5,2.75,2.71,2.24,2.29)

y1=y[1:6]                                                                                                       
x1=x[1:6]
y2 = y[6:13]
x2 = x[6:13]
y21 = y[13:14]
x21 = x[13:14]
y22 = y[14:16]
x22 = x[14:16]
y3 = y[16:20]
x3 = x[16:20]
y4 = y[20:22]
x4 = x[20:22]
y5 = y[22:24]
x5 = x[22:24]
y6 = y[24:29]
x6 = x[24:29]
x61 = x[29:30]
y61 = y[29:30]
y66 = y[30:31]
x66 = x[30:31]
y7 = y[31:33]
x7 = x[31:33]
#y77 = y[32:33]
#x77 = x[32:33]
y78 = y[33:34]
x78 = x[33:34]

n <- length(x)
length(y)
length(x)
plot(x, y, pch=20,main = paste("Interpolacion del perrito con", n, "puntos"),xlim=c(0,31),ylim=c(0,9))
lines(spline(x1, y1, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x2, y2, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x21, y21, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x22, y22, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x3, y3, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x4, y4, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x5, y5, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x6, y6, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x61, y61, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x66, y66, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x7, y7, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
#lines(spline(x77, y77, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x78, y78, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))