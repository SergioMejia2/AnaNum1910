require(PolynomF)
require(graphics)
library(graphicsQC)
rm(list=ls())
#    |  P1   |  P2   |  P3   |  P4   |  P5   |  P6   |  P7   |  P8   |  P9   |  P10  |  P11  |  P12  |  P13  |  P14  |  P15  |  P16  |  P17  |  P18  |  P19  |  P20  |  P21  |  P22  |  P23  |  P24  |  P25  |  P26  |  P27  |  P28  |  P29  |  P30  |  P31  |  P32  |  P33  |  P34  |  P35 |
x = c( 00.50 , 01.01 , 05.75 , 07.46 , 11.28 , 15.20 , 18.46 , 21.25 , 24.15 , 25.80 , 28.00 , 30.80 , 30.81 , 29.40 , 27.40 , 26.21 , 24.97 , 20.32 , 19.54 , 18.80 , 14.04 , 12.54 , 11.68 , 09.55 , 08.30 , 09.00 , 09.05 , 08.80 , 07.50 , 00.50)
y = c( 02.40 , 02.98 , 03.67 , 05.41 , 07.45 , 06.30 , 04.49 , 07.15 , 07.05 , 05.80 , 05.85 , 04.20 , 02.40 , 01.20 , 00.80 , 00.44 , 00.54 , 01.01 , 00.80 , 01.08 , 00.98 , 01.08 , 01.33 , 01.00 , 01.64 , 02.50 , 02.75 , 02.71 , 02.24 , 02.40)

x = x*0.95 #Ajuste a la imagen

y1 = y[1:7]   ; x1=x[1:7]
y2 = y[7:12]  ; x2 = x[7:12]
y3 = y[12:13] ; x3 = x[12:13]
y4 = y[13:15] ; x4 = x[13:15]
y5 = y[15:16] ; x5 = x[15:16]
y6 = y[16:19] ; x6 = x[16:19]
y7 = y[19:20] ; x7 = x[19:20]
y8 = y[20:25] ; x8 = x[20:25]
x9 = x[25:26] ; y9 = y[25:26]
y10 = y[26:27];x10 = x[26:27]
y11 = y[27:29];x11 = x[27:29]
y12 = y[29:30];x12 = x[29:30]

n <- length(x)
dog_c = 2

plot(x, y, pch=20,main = paste("Interpolacion del perrito con", n-1, "puntos"),xlim=c(0,31),ylim=c(0,9))
lines(spline(x1, y1, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x2, y2, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x3, y3, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x4, y4, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x5, y5, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x6, y6, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x7, y7, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x8, y8, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x9, y9, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x10, y10, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x11, y11, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x12, y12, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))

yD=c(3 , 3.7 , 3.9 , 4.5 , 5.7 , 6.69 , 7.12 , 6.7 , 4.45 , 7.0 , 6.1  , 5.6  , 5.87 , 5.15 , 4.1)                                                                                                       
xD=c(1 , 2.0 , 5.0 , 6.0 , 7.5 , 8.10 , 10.0 , 13  , 17.6 , 20  , 23.5 , 24.5 , 25.0 , 26.5 , 27.5)     
par(new=TRUE)
plot(xD, yD, pch=20,xlim=c(0,31),ylim=c(0,9),col="green")

ret1 = splinefun(x1,y1,method = "natural")
for(i in 1:9)
{
  val = ret1(xD[i])
  err = abs(yD[i]-val)/yD[i] * 100
  cat(xD[i],yD[i],val," Error: ",err,"\n")
}

ret2 = splinefun(x2,y2,method = "natural")
for(i in 9:15)
{
  val = ret2(xD[i])
  err = abs(yD[i]-val)/yD[i] * 100
  cat(xD[i],yD[i],val," Error: ",err,"\n")
}

#JACCARD
ac = 0
for(i in 1:15)
{
  for(j in 1:12)
  {
    #cat(x[j],xD[i]," y ",y[j],yD[i],"\n")
    if((x[j]-0.5 <= xD[i] && xD[i] <= x[j]+0.5) && (y[j]-0.5 <= yD[i] && yD[i] <= y[j]+0.5))
    {
      #cat(x[j],xD[i]," y ",y[j],yD[i],"\n")
      ac = ac+1
    }
  }
}
print(ac)