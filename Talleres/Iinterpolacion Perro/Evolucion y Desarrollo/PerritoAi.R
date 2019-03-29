require(PolynomF)
require(graphics)
library(graphicsQC)
rm(list=ls())
x = c(0.45, 1.46, 5.41, 7.42, 11.48, 18.36, 22.54, 24.83, 26.38, 27.94, 30.16, 30.83, 30.84, 29.45, 27.3, 26.91, 26.59, 26.21, 25.78, 24.97, 20.28, 19.54, 18.8, 14.04, 12.34, 11.43, 9.52, 8.35, 9.0, 9.05, 8.8 ,7.8 ,6.87,0.45)
y = c(2.24, 2.93, 3.53, 5.61, 7.59,  4.59 , 7.61 , 6.48 , 5.74 , 5.91 , 5.17 , 3.9 , 2.6 , 1.15 , 0.69, 0.55 , 0.59 , 0.44 , 0.52 , 0.41 , 1.01 , 0.8  , 1.08, 1.01 , 1.08 , 1.33 , 1.01, 1.64  , 2.5, 2.75,2.71,2.29,2.24,2.24)
y1=y[1:6]                                                                                                       
x1=x[1:6]
y2 = y[6:12]
x2 = x[6:12]
y21 = y[12:13]
x21 = x[12:13]
y22 = y[13:15]
x22 = x[13:15]
y3 = y[15:19]
x3 = x[15:19]
y4 = y[19:21]
x4 = x[19:21]
y5 = y[21:23]
x5 = x[21:23]
y6 = y[23:28]
x6 = x[23:28]
x61 = x[28:29]
y61 = y[28:29]
y66 = y[29:30]
x66 = x[29:30]
y7 = y[30:33]
x7 = x[30:33]
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