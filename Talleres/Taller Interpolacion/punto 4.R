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

p_h = 2.5
x0 = c(2,4)
y0 = c(83,193)
barylag (x0,y0,c(p_h))
barylag.multip(x0)
divided.differences(x0,y0,p_h)

x1 = c(1,2,4)
y1 = c(35,83,193)
barylag (x1,y1,c(p_h))
barylag.multip(x1)
divided.differences(x1,y1,p_h)

x2 = c(2,4,5)
y2 = c(83,193,215)
barylag (x2,y2,c(p_h))
barylag.multip(x2)
divided.differences(x2,y2,p_h)

x3 = c(2,3,4)
y3 = c(83,153,193)
barylag (x3,y3,c(p_h))
barylag.multip(x3)
divided.differences(x3,y3,p_h)

x4 = c(1,2,3,4,5)
y4 = c(35,83,153,193,215)
barylag (x4,y4,c(p_h))
barylag.multip(x4)
divided.differences(x4,y4,p_h)

plot(x4,y4)
