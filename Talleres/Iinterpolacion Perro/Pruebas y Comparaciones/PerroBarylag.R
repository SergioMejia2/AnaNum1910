require(pracma)

#    |  P1   |  P2   |  P3   |  P4   |  P5   |  P6   |  P7   |  P8   |  P9   |  P10  |  P11  |  P12  |  P13  |  P14  |  P15  |  P16  |  P17  |  P18  |  P19  |  P20  |  P21  |  P22  |  P23  |  P24  |  P25  |  P26  |  P27  |  P28  |  P29  |  P30  |  P31  |  P32  |  P33  |  P34  |  P35 |
x = c( 00.50 , 01.01 , 05.85 , 07.46 , 11.28 , 15.20 , 18.46 , 21.25 , 24.15 , 25.80 , 28.00 , 30.80 , 30.81 , 29.40 , 27.40 , 26.21 , 24.97 , 20.32 , 19.54 , 18.80 , 14.04 , 12.54 , 11.68 , 09.55 , 08.30 , 09.10 , 08.85 , 07.80 , 00.50)
y = c( 02.40 , 02.95 , 03.86 , 05.41 , 07.45 , 06.30 , 04.49 , 07.15 , 07.05 , 05.80 , 05.85 , 04.20 , 02.40 , 01.20 , 00.80 , 00.44 , 00.54 , 01.01 , 00.80 , 01.08 , 00.98 , 01.08 , 01.33 , 01.00 , 01.64 , 02.65 , 02.70 , 02.24 , 02.40)

x = x*0.95 #Ajuste a la imagen
op = 0
plot(x,y, pch=20, cex=1, col = "black", asp=1,xlab="X", ylab="Y", main="Perro")

Graficar<-function(x0, xn){
  xi = x[x0:xn]
  yi = y[x0:xn]
  x <- seq(x[x0], x[xn], len=100)
  y <- barylag(xi, yi, x)
  op = (8*length(xi)-2)
  #print(x,y)
  lines(x, y, col="red")
  return(op)
}
op = op + Graficar(1,7)
op = op + Graficar(7,12)
op = op + Graficar (12,13)
op = op + Graficar (13,15)
op = op + Graficar(15,19)
op = op + Graficar(19,20)
op = op + Graficar(20,25)
op = op + Graficar (25,26)
op = op + Graficar (26,28)
op = op + Graficar (28,29)

cat("Numero de operaciones: ",op,"\n")
#Graficar(9,10)