y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                                                                                       
x=c(1,2,5,6,7,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)     
length(x)
length(y)


plot(x,y,  pch = 19, cex=0.5, col= "red",xlim=c(0,31),ylim=c(0,10))

x1=x[1:4]
y1=y[1:4]
polyAjuste1 = poly.calc(x1,y1)
curve(polyAjuste1,from=1,to=6,add=T, lwd=1)

x2=x[4:6]
y2=y[4:6]
polyAjuste2 = poly.calc(x2,y2)
curve(polyAjuste2,from=6,to=8.1,add=T, lwd=1)

x3=x[6:9]
y3=y[6:9]
polyAjuste3 = poly.calc(x3,y3)
curve(polyAjuste3,from=8.1,to=17.6,add=T, lwd=1)

x4=x[9:12]
y4=y[9:12]
polyAjuste4 = poly.calc(x4,y4)
curve(polyAjuste4,from=17.6,to=24.5,add=T, lwd=1)

