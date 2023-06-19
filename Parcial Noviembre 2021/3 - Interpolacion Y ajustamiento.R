# 3 - Interpolacion y ajustamiento

x_dado = c(1.6579, 3.8119, 4.1155, 5.1111, 6.0537, 6.2298, 11.6898, 11.7199, 12.7634)
y_dado = c(0.1111, 0.2222, 0.3333, 0.4444, 0.5556, 0.6667, 0.7778, 0.8889, 1)
x = 12.2416 
  
# 3.1 ----
Interp_Lagrange = function (x, x_dado, y_dado){
  polinomio = 0
  n = length(x_dado)
  aux = c(rep(1,n))
  for (k in 1:n){
    for(i in 1:n){
      if(k != i){
        aux[k] = aux[k] * ((x - x_dado[i]) / (x_dado[k] - x_dado[i]))
      }
    }
    polinomio = polinomio + (y_dado[k] * aux[k])
  }
  return(polinomio)
}

Interp_Lagrange(x, x_dado, y_dado)
#2.768489

# Ultimos 3 pares de datos
x_dado = c(11.6898, 11.7199, 12.7634)
y_dado = c(0.7778, 0.8889, 1)

Interp_Lagrange(x, x_dado, y_dado) #1.85335

# Ultimas dos observaciones
x_dado = c(11.7199, 12.7634)
y_dado = c(0.8889, 1)

Interp_Lagrange(x, x_dado, y_dado) # 0.9444447

#Mejor con dos. Lagrange problemas en extremos 







# 3.2 ----
x_dado = c(0.0227,0.0817,0.3147,0.5258,0.7502,0.8877,1.3583,1.3716,1.5854,1.6288,2.8558,3.0106,5.1854)
y_dado = c(0.0769,0.1538,0.2308,0.3077,0.3846,0.4615,0.5385,0.6154,0.6923,0.7692,0.8462,0.9231,1)
x = 0.0522

TrazadorCubicoNatural = function(x,y)
{
  n = length(y)
  j = n - 1
  a = y
  b = c(rep(NA,n))
  c = c(rep(NA,n))
  d = c(rep(NA,n))
  A = c(rep(NA,n))
  h = c(rep(NA,n))
  l = c(rep(NA,n))
  u = c(rep(NA,n))
  z = c(rep(NA,n))
  
  for (i in 1:j) { 
    h[i] = x[i + 1] - x[i]
  }
  
  for (i in 1:j) { 
    if(i != 1){
      A[i] = (3 * (a[i + 1] - a[i])/(h[i])) - (3 * (a[i] - a[i - 1]) /h[i - 1])
    }
  }
  
  l[1] = 1
  u[1] = 0
  z[1] = 0
  #Paso 4
  for (i in 2:j) {
    l[i] = 2 * (x[i + 1] - x[i - 1]) - h[i - 1] * u[i - 1]
    u[i] = h[i]/l[i]
    z[i] = (A[i] - h[i - 1] * z[i - 1])/l[i]
  }
  
  l[n] = 1
  z[n] = 0
  c[n] = 0
  
  for (i in j:1) {
    c[i] = z[i] - u[i] * c[i + 1]
    b[i] = (a[i + 1] - a[i])/h[i] - h[i] * (c[i + 1] + 2*c[i])/3
    d[i] = (c[i + 1] - c[i])/(3*h[i])
  }
  
  results=matrix(data=c(a,b,c,d),ncol = 4)
  colnames(results)=c("a","b","c","d")
  results=matrix(data=c(a,b,c,d),ncol = 4)
  colnames(results)=c("a","b","c","d")
  print(paste('S1(x) =', round(results[1,1],4), '+', round(results[1,2],4), '(xo - ', x[1],') +', round(results[1,3],4),'(xo - ', x[1],')^2 + ',round(results[1,4],4),'*(xo - (', x[1],')^3 para todo x perteneciente a [",x[i],",",x[i+1],"]",sep = '))
  print(paste('S2(x) =', round(results[2,1],4), '+', round(results[2,2],4), '(xo - ', x[2],') +', round(results[2,3],4),'(xo - ', x[2],')^2 + ',round(results[2,4],4),'*(xo - (', x[2],')^3'))
  print(paste('S3(x) =', round(results[3,1],4), '+', round(results[3,2],4), '(xo - ', x[3],') +', round(results[3,3],4),'(xo - ', x[3],')^2 + ',round(results[3,4],4),'*(xo - (', x[3],')^3'))
  print(paste('S4(x) =', round(results[4,1],4), '+', round(results[4,2],4), '(xo - ', x[4],') +', round(results[4,3],4),'(xo - ', x[4],')^2 + ',round(results[4,4],4),'*(xo - (', x[4],')^3'))
  print(paste('S5(x) =', round(results[5,1],4), '+', round(results[5,2],4), '(xo - ', x[5],') +', round(results[5,3],4),'(xo - ', x[5],')^2 + ',round(results[5,4],4),'*(xo - (', x[5],')^3'))
  print(paste('S6(x) =', round(results[6,1],4), '+', round(results[6,2],4), '(xo - ', x[6],') +', round(results[6,3],4),'(xo - ', x[6],')^2 + ',round(results[6,4],4),'*(xo - (', x[6],')^3'))
  print(paste('S7(x) =', round(results[7,1],4), '+', round(results[7,2],4), '(xo - ', x[7],') +', round(results[7,3],4),'(xo - ', x[7],')^2 + ',round(results[7,4],4),'*(xo - (', x[7],')^3'))
  print(paste('S8(x) =', round(results[8,1],4), '+', round(results[8,2],4), '(xo - ', x[8],') +', round(results[8,3],4),'(xo - ', x[8],')^2 + ',round(results[8,4],4),'*(xo - (', x[8],')^3'))
  print(paste('S9(x) =', round(results[9,1],4), '+', round(results[9,2],4), '(xo - ', x[9],') +', round(results[9,3],4),'(xo - ', x[9],')^2 + ',round(results[9,4],4),'*(xo - (', x[9],')^3'))
  print(paste('S10(x) =', round(results[10,1],4), '+', round(results[10,2],4), '(xo - ', x[10],') +', round(results[10,3],4),'(xo - ', x[10],')^2 + ',round(results[10,4],4),'*(xo - (', x[10],')^3'))
  print(paste('S11(x) =', round(results[11,1],4), '+', round(results[11,2],4), '(xo - ', x[11],') +', round(results[11,3],4),'(xo - ', x[11],')^2 + ',round(results[11,4],4),'*(xo - (', x[11],')^3'))
  print(paste('S12(x) =', round(results[12,1],4), '+', round(results[12,2],4), '(xo - ', x[12],') +', round(results[12,3],4),'(xo - ', x[12],')^2 + ',round(results[12,4],4),'*(xo - (', x[12],')^3'))
}

TrazadorCubicoNatural(x_dado,y_dado)

xp = c(1.6579, 3.8119, 4.1155, 5.1111, 6.0537, 6.2298, 11.6898, 11.7199, 12.7634)
fp = c(0.1111, 0.2222, 0.3333, 0.4444, 0.5556, 0.6667, 0.7778, 0.8889, 1)
x = 12.2416 
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
InterpolacionCubicoNatural(x, xp, fp)
# 3.3 ----
x_dado <- c(1.6579, 3.8119, 4.1155, 5.1111, 6.0537, 6.2298, 11.6898, 11.7199, 12.7634)
y_dado <- c(0.1111, 0.2222, 0.3333, 0.4444, 0.5556, 0.6667, 0.7778, 0.8889, 1)
data = data.frame(x_dado,y_dado)  
data
media = mean(data$x_dado)
desvioestandar = sd(data$x_dado)



Modelo = nls(y_dado ~ pnorm(x_dado,mu,sigma), 
             data = data,
             start = list(mu = media, sigma = desvioestandar))
Modelo

Aj = sapply(12.2416, function(x) pnorm(x,mean = 7.017, sd = 4.024376470)) 
Aj