InterpolacionCubicoNatural <- function(x, xp, fp){
  n = length(xp)
  a = fp
  b = c(rep(NA,n))
  c = c(rep(NA,n))
  d = c(rep(NA,n))
  
  A = c(rep(NA,n)) 
  h = c(rep(NA,n))
  l = c(rep(NA,n))
  u = c(rep(NA,n))
  z = c(rep(NA,n))
  
  
  for (i in 0:(n-1)) {
    h[i] = xp[i+1] - xp[i]
  }
  
  for (i in 1:(n-1)) {
    if (i != 1) {
      A[i] = (3/h[i])*(a[i+1] - a[i]) - (3/(h[i-1]))*(a[i] - a[i-1])
    }
  }
  
  l[1] = 1
  u[1] = 0
  z[1] = 0
  
  for (i in 2:(n-1)) { 
    l[i] = 2*(xp[i+1] - xp[i-1]) - h[i-1]*u[i-1]
    u[i] = h[i]/l[i]
    z[i] = (A[i] - h[i-1]*z[i-1])/l[i]
  }
  
  l[n] = 1
  z[n] = 0
  c[n] = 0
  
  for (i in (n-1):1){
    c[i] = z[i] - u[i]*c[i+1]
    b[i] = (a[i+1] - a[i])/h[i] - h[i]*(c[i+1] + 2*c[i])/3
    d[i] = (c[i+1] - c[i])/(3*h[i])
  }
  # Interpolación
  for (i in 1:(n-1)) {
    if ((xp[i] <= x) & (xp[i+1] >= x))  {
      I = i
    }
  }
  INT = fp[I] + b[I]*(x - xp[I]) + c[I]*(x - xp[I])^2 + d[I]*(x -xp[I])^3
  return(INT)
}