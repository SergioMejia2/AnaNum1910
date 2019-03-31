require(PolynomF)
require(graphics)
library(graphicsQC)
rm(list=ls())
#    |  P1   |  P2   |  P3   |  P4   |  P5   |  P6   |  P7   |  P8   |  P9   |  P10  |  P11  |  P12  |  P13  |  P14  |  P15  |  P16  |  P17  |  P18  |  P19  |  P20  |  P21  |  P22  |  P23  |  P24  |  P25  |  P26  |  P27  |  P28  |  P29  |  P30  |  P31  |  P32  |  P33  |  P34  |  P35 |
x = c( 00.50 , 01.01 , 05.85 , 07.46 , 11.28 , 15.20 , 18.46 , 21.25 , 24.15 , 25.80 , 28.00 , 30.80 , 30.81 , 29.40 , 27.40 , 26.21 , 24.97 , 20.32 , 19.54 , 18.80 , 14.04 , 12.54 , 11.68 , 09.55 , 08.30 , 09.10 , 08.85 , 07.80 , 00.50)
y = c( 02.40 , 02.95 , 03.86 , 05.41 , 07.45 , 06.30 , 04.49 , 07.15 , 07.05 , 05.80 , 05.85 , 04.50 , 02.40 , 01.20 , 00.80 , 00.44 , 00.54 , 01.01 , 00.80 , 01.08 , 00.98 , 01.08 , 01.33 , 01.00 , 01.64 , 02.65 , 02.70 , 02.24 , 02.40)

x = x*0.95 #Ajuste a la imagen

print(x)
print(y)

y1 = y[1:7]   ; x1=x[1:7]
y2 = y[7:12]  ; x2 = x[7:12]
y3 = y[12:14] ; x3 = x[12:14]
y4 = y[14:15] ; x4 = x[14:15]
y5 = y[15:18] ; x5 = x[15:18]
y7 = y[18:20] ; x7 = x[18:20]
y8 = y[20:25] ; x8 = x[20:25]
#x9 = x[25:26] ; y9 = y[25:26]
y10 = y[25:26];x10 = x[25:26]
y11 = y[26:28];x11 = x[26:28]
y12 = y[28:29];x12 = x[28:29]

n <- length(x)
dog_c = 2

p = spline(y3,x3, n = 201)
i = p$x
p$x = p$y
p$y = i

plot(x, y, pch=20,main = paste("Interpolacion del perrito con", n-1, "puntos"),xlim=c(0,31),ylim=c(0,9))

lines(spline(x2, y2, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
#lines(spline(x3, y3, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x4, y4, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x5,y5, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
#lines(spline(x6, y6, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x7, y7, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x8, y8, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x10, y10, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x11, y11, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x12, y12, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(p, col = dog_c)
lines(spline(x1, y1, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))

op = 0
op = 26*(length(x1) +length(x2)+ length(x3)+ length(x4) + length(x5)+ length(x7) 
         + length(x8)+ length(x10) +length(x11)+ length(x12)) - (32*10)
cat("Numero de operaciones: ",op,"\n")


cat("Error Puntos Originales")
cat("\n")
cat("\n")
#Puntos dados por la profesora
yD=c(3 , 3.7 , 3.9 , 4.5 , 5.7 , 6.69 , 7.12 , 6.7 , 4.45 , 7.0 , 6.1  , 5.6  , 5.87 , 5.15 , 4.1)                                                                                                       
xD=c(1 , 2.0 , 5.0 , 6.0 , 7.5 , 8.10 , 10.0 , 13  , 17.6 , 20  , 23.5 , 24.5 , 25.0 , 26.5 , 27.5)     

par(new=TRUE)
plot(xD, yD, pch=20,xlim=c(0,31),ylim=c(0,9),col="green")

errs = 0
it = 1
#cota de error1
ret1 = splinefun(x1,y1,method = "natural")
for(i in 1:9)
{
  val = ret1(xD[i])
  err = abs(yD[i]-val)/yD[i] * 100
  cat(xD[i],yD[i],val," Error: ",err,"\n")
  #cat(xD[i]," & ",yD[i]," & ",val," & ",err,"\\%","\\","\\","\n")
}
#cota de error2
ret2 = splinefun(x2,y2,method = "natural")
for(i in 9:15)
{
  val = ret2(xD[i])
  err = abs(yD[i]-val)/yD[i] * 100
  cat(xD[i],yD[i],val," Error: ",err,"\n")
}
cat("\n")
cat("\n")
cat("\n")


cat("Error Puntos Ilustrator CC 2019","\n")
cat("\n")
cat("\n")
#Puntos originales de Ilustrator CC 2019
xI = c(0.45, 1.46, 5.41, 7.42, 11.48, 18.36, 22.54, 24.83, 26.38, 27.94, 30.16, 30.83, 29.45, 27.3, 26.91, 26.59, 26.21, 25.78, 24.97, 20.28, 19.54, 18.8, 14.04, 12.34, 11.43, 9.52, 8.25, 8.96, 6.87, 0.45)
yI = c(2.24, 3.43, 3.53, 5.61,  7.59,  4.59,  7.61,  6.48,  5.74,  5.91, 5.17 , 3.34 , 1.15 , 0.69, 0.55 , 0.59 , 0.44 , 0.46 , 0.52 , 1.01 , 0.8  , 1.08, 1.01 , 1.08 , 1.33 , 1.01, 1.64, 2.81, 2.24, 2.24)
xI = xI*0.95 #Ajuste a la imagen

par(new=TRUE)
points(xI, yI, pch=20,xlim=c(0,31),ylim=c(0,9),col="blue")

#cota de error1
ret11 = splinefun(x1,y1,method = "natural")
for(i in 1:6)
{
  val = ret11(xI[i])
  err = abs(yI[i]-val)/yI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
  errs[it] = err
  it = it + 1
  #cat(xI[i]," & ",yI[i]," & ",val," & ",err,"\\%","\\","\\","\n")
}

#cota de error2
ret22 = splinefun(x2,y2,method = "natural")
for(i in 7:11)
{
  val = ret22(xI[i])
  err = abs(yI[i]-val)/yI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
  errs[it] = err
  it = it + 1
}

ret3 = splinefun(y3,x3,method = "natural")
for(i in 12:12)
{
  val = ret3(yI[i])
  err = abs(xI[i]-val)/xI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
  errs[it] = err
  it = it + 1
}

#cota de error4
ret4 = splinefun(x4,y4,method = "natural")
for(i in 13:13)
{
  val = ret4(xI[i])
  err = abs(yI[i]-val)/yI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
}

#cota de error5
ret5 = splinefun(x5,y5,method = "natural")
for(i in 14:20)
{
  val = ret5(xI[i])
  err = abs(yI[i]-val)/yI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
  errs[it] = err
  it = it + 1
}


#cota de error7
ret7 = splinefun(x7,y7,method = "natural")
for(i in 21:22)
{
  val = ret7(xI[i])
  err = abs(yI[i]-val)/yI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
  errs[it] = err
  it = it + 1
}

#cota de error8
ret8 = splinefun(x8,y8,method = "natural")
for(i in 23:27)
{
  val = ret8(xI[i])
  err = abs(yI[i]-val)/yI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
  errs[it] = err
  it = it + 1
}

#cota de error11
ret111 = splinefun(x11,y11,method = "natural")
for(i in 28:28)
{
  val = ret111(xI[i])
  err = abs(yI[i]-val)/yI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
  errs[it] = err
  it = it + 1
}

#cota de error12
ret12 = splinefun(x12,y12,method = "natural")
for(i in 29:29)
{
  val = ret12(xI[i])
  err = abs(yI[i]-val)/yI[i] * 100
  cat(xI[i],yI[i],val," Error: ",err,"\n")
  errs[it] = err
  it = it + 1
}

#Analisis de la cota de error
plot(seq(1,28),errs,pch=20,main = "Gráfica de errores",xlab="Puntos",ylab="Error porcental (%)", ylim = c(0,20))
lines(seq(1,28),errs)
abline(a = 13, b = 0, col="red",lty=2)
qqnorm(errs)
qqline(errs)

cat("\n")
cat("\n")
cat("\n")

#JACCARD1
ac1 = 0
for(i in 1:15)
{
  for(j in 1:12)
  {
    #cat(x[j],xD[i]," y ",y[j],yD[i],"\n")
    if((x[j]-0.1 <= xD[i] && xD[i] <= x[j]+0.1) && (y[j]-0.1 <= yD[i] && yD[i] <= y[j]+0.1))
    {
      #cat(x[j],xD[i]," y ",y[j],yD[i],"\n")
      ac1 = ac1+1
    }
  }
}
cat("Aciertos profesora: ", ac1, "\n")

#JACCARD2
ac2 = 0
for(i in 1:29)
{
  for(j in 1:(n-1))
  {
    if((x[j]-0.1 <= xI[i] && xI[i] <= x[j]+0.1) && (y[j]-0.1 <= yI[i] && yI[i] <= y[j]+0.1))
    {
      ac2 = ac2+1
    }
  }
}
cat("Aciertos Illustrator: ", ac2, "\n")