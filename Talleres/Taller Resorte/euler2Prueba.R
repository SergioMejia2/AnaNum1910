euler2 = function(a,b,N, alp, bet){
  
  h = (b-a)/N
  
  t =c()
  u = c()
  y = c()
  
  t[1] = a
  y[1] = alp
  u[1] = bet
  
  it = 2
  while(it <= N+1)
  {
    t[it]=t[it-1]+h
    y[it]=y[it-1]+h*u[it-1]
    u[it]=u[it-1]+h*(-64*y[it-1])
    it = it + 1
  }
  cat(t[N+1]," ", y[N+1], " ",u[N+1],"\n")
}

euler2(0,5,1e6,2/3,-4/3)  #0.4337157 