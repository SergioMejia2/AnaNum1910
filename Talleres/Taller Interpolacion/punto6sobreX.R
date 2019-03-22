s1 <- function(x)
{
  y0 = 0
  it = 1
  while (it <= length(x))
  {
    y0[it] = 1/x[it]
    it = it + 1
  }
  return(y0)
}

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

x0 = c(-1/4,-1/8,1/16,1/8,1/4)
x1 = c(1/2,-1/4,-1/8,1/16,1/8,1/4)
x2 = c(-1/4,-1/8,1/16,1/8,1/4,1/2)
x3 = c(-1/4,-1/8,-1/16,1/16,1/8,1/4)
x4 = c(-1/2,-1/4,-1/8,1/16,1/8,1/4,1/2)
y0 = s1(x0)
y1 = s1(x1)
y2 = s1(x2)
y3 = s1(x3)
y4 = s1(x4)

barylag (x0,y0,c(0))
barylag.multip(x0)
divided.differences(x0,y0,0)

barylag (x1,y1,c(0))
barylag.multip(x1)
divided.differences(x1,y1,0)

barylag (x2,y2,c(0))
barylag.multip(x2)
divided.differences(x2,y2,0)

barylag (x3,y3,c(0))
barylag.multip(x3)
divided.differences(x3,y3,0)

barylag (x4,y4,c(0))
barylag.multip(x4)
divided.differences(x4,y4,0)
