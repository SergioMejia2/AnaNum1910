require(PolynomF)
require(graphics)
library(graphicsQC)
rm(list=ls())
#    |  P1   |  P2   |  P3   |  P4   |  P5   |  P6   |  P7   |  P8   |  P9   |  P10  |  P11  |  P12  |  P13  |  P14  |  P15  |  P16  |  P17  |  P18  |  P19  |  P20  |  P21  |  P22  |  P23  |  P24  |  P25  |  P26  |  P27  |  P28  |  P29  |  P30  |  P31  |  P32  |  P33  |  P34  |  P35 |
x = c( 00.50 , 01.01 , 05.75 , 07.46 , 11.28 , 15.20 , 18.46 , 21.25 , 24.15 , 25.80 , 28.00 , 30.80 , 30.81 , 29.45 , 27.40 , 26.91 , 26.59 , 26.21 , 24.97 , 22.70 , 20.32 , 19.54 , 18.80 , 14.04 , 12.54 , 11.68 , 09.55 , 08.30 , 09.00 , 09.05 , 08.80 , 07.50 , 00.50)
y = c( 02.40 , 02.98 , 03.67 , 05.41 , 07.45 , 06.30 , 04.49 , 07.15 , 07.05 , 05.80 , 05.85 , 04.20 , 02.40 , 01.15 , 00.80 , 00.55 , 00.59 , 00.44 , 00.54 , 00.95 , 01.01 , 00.80 , 01.08 , 00.98 , 01.08 , 01.33 , 01.00 , 01.64 , 02.50 , 02.75 , 02.71 , 02.24 , 02.40)

y1=y[1:7]                                                                                                       
x1=x[1:7]
y2 = y[7:12]
x2 = x[7:12]
y21 = y[12:13]
x21 = x[12:13]
y22 = y[13:15]
x22 = x[13:15]
y3 = y[15:18]
x3 = x[15:18]
y4 = y[18:21]
x4 = x[18:21]
y5 = y[21:23]
x5 = x[21:23]
y6 = y[23:28]
x6 = x[23:28]
x61 = x[28:29]
y61 = y[28:29]
y66 = y[29:30]
x66 = x[29:30]
y7 = y[30:32]
x7 = x[30:32]
#y77 = y[32:33]
#x77 = x[32:33]
y78 = y[32:33]
x78 = x[32:33]

n <- length(x)
length(y)
length(x)
dog_c = 2


plot(x, y, pch=20,main = paste("Interpolacion del perrito con", n-1, "puntos"),xlim=c(0,31),ylim=c(0,9))
lines(spline(x1, y1, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x2, y2, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x21, y21, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x22, y22, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x3, y3, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x4, y4, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x5, y5, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x6, y6, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x61, y61, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x66, y66, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x7, y7, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
#lines(spline(x77, y77, n = 201), col = 2,xlim=c(0,31),ylim=c(0,9))
lines(spline(x78, y78, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))

yD=c(3 , 3.7 , 3.9 , 4.5 , 5.7 , 6.69 , 7.12 , 6.7 , 4.45 , 7.0 , 6.1  , 5.6  , 5.87 , 5.15 , 4.1  , 4.3 , 4.1 ,3 )                                                                                                       
xD=c(1 , 2.0 , 5.0 , 6.0 , 7.5 , 8.10 , 10.0 , 13  , 17.6 , 20  , 23.5 , 24.5 , 25.0 , 26.5 , 27.5 , 28  , 29  ,30)     

ret1 = splinefun(x1,y1,method = "natural")
for(i in 1:9)
{
  val = ret1(xD[i])
  err = abs(yD[i]-val)/yD[i] * 100
  cat(xD[i],yD[i],val," Error: ",err,"\n")
}
