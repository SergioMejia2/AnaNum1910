require(pracma)

barylag.multip <- function(x)
{
  n = length(x)
  return((n+1)^2)
}

divided.differences <- function(x, y, x0) {
  require(rSymPy)
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  f <- as.character(round(q[1,1], 5))
  fi <- ''
  it = 0
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (q[j,i-1] - q[j-1,i-1]) / (x[j] - x[j-i+1])
      it = it + 1
    }
    fi <- paste(fi, '*(x - ', x[i-1], ')', sep = '', collapse = '')
    
    f <- paste(f, ' + ', round(q[i,i], 5), fi, sep = '', collapse = '')
  }
  
  x <- Var('x')
  sympy(paste('e = ', f, collapse = '', sep = ''))
  approx <- sympy(paste('e.subs(x, ', as.character(x0), ')', sep = '', collapse = ''))
  
  return(list('Approximation from Interpolation'=as.numeric(approx),'Multiplications'=it))
}

ex <- function(x)
{
  y0 = 0
  it = 1
  while (it <= length(x))
  {
    y0[it] = exp(1)^x[it]
    it = it + 1
  }
  return(y0)
}


x1 = c(0.125,0.250,0.625,0.750,0.875)
y1 = ex(x1)
barylag (x1,y1,c(0.5))
barylag.multip(x1)
divided.differences(x1,y1,0.5)

x2 = c(0.125,0.250,0.375,0.625,0.750,0.875)
y2 = ex(x2)
barylag (x2,y2,c(0.5))
barylag.multip(x2)
divided.differences(x2,y2,0.5)

x3 = c(0.000,0.125,0.250,0.625,0.750,0.875)
y3 = ex(x3)
barylag (x3,y3,c(0.5))
barylag.multip(x3)
divided.differences(x3,y3,0.5)

x4 = c(0.125,0.250,0.625,0.750,0.875,1.000)
y4 = ex(x4)
barylag (x4,y4,c(0.5))
barylag.multip(x4)
divided.differences(x4,y4,0.5)

xt = c(0.000,0.125,0.250,0.375,0.625,0.750,0.875,1.000)
yt = ex(xt)
barylag (xt,yt,c(0.5))
barylag.multip(xt)
divided.differences(xt,yt,0.5)