# Interpolacion y Ajustamiento  

x_dado = c(0.0227,0.0817,0.3147,0.5258,0.7502,0.8877,1.3583,1.3716,1.5854,1.6288,2.8558,3.0106,5.1854)
y_dado = c(0.0769,0.1538,0.2308,0.3077,0.3846,0.4615,0.5385,0.6154,0.6923,0.7692,0.8462,0.9231,1)
x = 0.0522

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
# 0.3317224202
# Polinomio de grado 12, el polinomio de mayor grado posible, ya que con n datos
# obtendremos un polinomio de grado n-1. Es de notar que la interpolacion de Lagrange
# necesita contar siempre con un intervalo mayor y menos al que estamos calculando, 
# de no ser asi, seria extrapolacion. 

# Este X = 0.0522 no se puede aproximar usando los ultimos 4 o dos pares de datos
# Por que X no se encuentra en el intervalo. Se deberian utilizar los primeros 4 0 2
# De ser asi, probablemente obtendriamos un valor mas preciso que con el polinomio 
# de grado 13, por que como se comento, lagrange tiene problemas en los extremos. 

x_dado = c(0.0227,0.0817,0.3147,0.5258)
y_dado = c(0.0769,0.1538,0.2308,0.3077)

Interp_Lagrange(x, x_dado, y_dado) #0.1197970709

# Si hubiera utilizado los ultimos 4 obtendria
x_dado = c(1.6288,2.8558,3.0106,5.1854)
y_dado = c(0.7692,0.8462,0.9231,1)
Interp_Lagrange(x, x_dado, y_dado) # 3.941664443
# Que como podemos observar es incorrecto

x_dado = c(0.0227,0.0817)
y_dado = c(0.0769,0.1538)

Interp_Lagrange(x, x_dado, y_dado) #0.11535

x_dado = c(3.0106,5.1854)
y_dado = c(0.9231,1)
Interp_Lagrange(x, x_dado, y_dado) # 0.8184922384

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
  print(paste('S1(x) =', round(results[1,1],4), '+', round(results[1,2],4), '(xo - ', x[1],') +', round(results[1,3],4),'(xo - ', x[1],')^2 + ',round(results[1,4],4),'*(xo - (', x[1],')^3'))
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

#[1] "S1(x) = 0.0769 + 1.4171 (xo -  0.0227 ) + 0 (xo -  0.0227 )^2 +  -32.6604 *(xo - ( 0.0227 )^3"
#[1] "S2(x) = 0.1538 + 1.076 (xo -  0.0817 ) + -5.7809 (xo -  0.0817 )^2 +  11.0779 *(xo - ( 0.0817 )^3"
#[1] "S3(x) = 0.2308 + 0.1863 (xo -  0.3147 ) + 1.9626 (xo -  0.3147 )^2 +  -5.3041 *(xo - ( 0.3147 )^3"
#[1] "S4(x) = 0.3077 + 0.3059 (xo -  0.5258 ) + -1.3965 (xo -  0.5258 )^2 +  6.9548 *(xo - ( 0.5258 )^3"
#[1] "S5(x) = 0.3846 + 0.7297 (xo -  0.7502 ) + 3.2855 (xo -  0.7502 )^2 +  -32.9109 *(xo - ( 0.7502 )^3"
#[1] "S6(x) = 0.4615 + -0.2334 (xo -  0.8877 ) + -10.2903 (xo -  0.8877 )^2 +  23.659 *(xo - ( 0.8877 )^3"
#[1] "S7(x) = 0.5385 + 5.8003 (xo -  1.3583 ) + 23.1116 (xo -  1.3583 )^2 +  -1841.3912 *(xo - ( 1.3583 )^3"
#[1] "S8(x) = 0.6154 + 5.4379 (xo -  1.3716 ) + -50.3599 (xo -  1.3716 )^2 +  124.4517 *(xo - ( 1.3716 )^3"
#[1] "S9(x) = 0.6923 + 0.9702 (xo -  1.5854 ) + 29.4634 (xo -  1.5854 )^2 +  -253.2645 *(xo - ( 1.5854 )^3"
#[1] "S10(x) = 0.7692 + 2.0965 (xo -  1.6288 ) + -3.5116 (xo -  1.6288 )^2 +  1.5111 *(xo - ( 1.6288 )^3"#
#[1] "S11(x) = 0.8462 + 0.304 (xo -  2.8558 ) + 2.0507 (xo -  2.8558 )^2 +  -5.2023 *(xo - ( 2.8558 )^3"
#[1] "S12(x) = 0.9231 + 0.5649 (xo -  3.0106 ) + -0.3652 (xo -  3.0106 )^2 +  0.056 *(xo - ( 3.0106 )^3"


InterpolacionCubicoNatural <- function(x, x_dado, y_dado){
  n = length(x_dado)
  a = y_dado
  b = c(rep(NA,n))
  c = c(rep(NA,n))
  d = c(rep(NA,n))
  
  A = c(rep(NA,n)) 
  h = c(rep(NA,n))
  l = c(rep(NA,n))
  u = c(rep(NA,n))
  z = c(rep(NA,n))
  
  
  for (i in 0:(n-1)) {
    h[i] = x_dado[i+1] - x_dado[i]
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
    l[i] = 2*(x_dado[i+1] - x_dado[i-1]) - h[i-1]*u[i-1]
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
    if ((x_dado[i] <= x) & (x_dado[i+1] >= x))  {
      I = i
    }
  }
  INT = y_dado[I] + b[I]*(x - x_dado[I]) + c[I]*(x - x_dado[I])^2 + d[I]*(x - x_dado[I])^3
  return(INT)
}
InterpolacionCubicoNatural(x, x_dado, y_dado)

#Teniendo en cuenta que el punto que se pide interpolar es el 0.0522, la probabilidad, debería estar
#entre 0.0769 y 0.1538. Entonces, comparando las imterpolaciones, se puede notar que, la interpolación de
#Cubi Spline está dentro de este intervalo y la de Newton utilizando todos los datos no. Esto indica, que es la aproximación óptima para hacer,
#es la de Cubic Spline. Se puede explicar porque la interpolación de lagrange/newton en los extremos no es suave, en cambio #Cubic Spline si.
#Tmbién se podría utilizar interpolación de Newton, siempre y cuando se utilicen la menor cantidad de datos de la tabla posibles

# 3.3 ----
x_dado <- c(0.0227,0.0817,0.3147,0.5258,0.7502,0.8877,1.3583,1.3716,1.5854,1.6288,2.8558,3.0106,5.1854)
y_dado <- c(0.0769,0.1538,0.2308,0.3077,0.3846,0.4615,0.5385,0.6154,0.6923,0.7692,0.8462,0.9231,1)
data = data.frame(x_dado,y_dado)  
data
media = mean(data$x_dado)
desvioestandar = sd(data$x_dado)



Modelo = nls(y_dado ~ pnorm(x_dado,mu,sigma), 
             data = data,
             start = list(mu = media, sigma = desvioestandar))
Modelo

Aj = sapply(0.0522, function(x) pnorm(x,mean = 1.092, sd = 1.044)) 
Aj #0.1596306574

#En este caso la aproximación es más precisa que por Lagrange pero no logra ser igual que
#Cubic Spline. Esto es porque en ajustamiento no busca que la linea pase por todos los puntos
#haciendo que en algunas partes, la aproximación se aleje del resultado que busco.

# 3.4 ----
library(ggplot2)
library(tidyverse)
x <-  seq(min(data$x), to = max(data$x), length.out = 1000)

P = sapply(x, function(x) Interp_Lagrange(x, x_dado, y_dado))

dataP = tibble(x,P)



CS = sapply(x,function(x) InterpolacionCubicoNatural(x, x_dado, y_dado))

dataCS = tibble(x,CS)

DN = sapply(x, function(x) pnorm(x, 1.092, 1.044))
dataDN = tibble(x, DN)




#PRIMERO GRAFICO AJUSTAMIENTO Y CUBIC SPLINE PARA VER MEJOR ESTOS 2 MÉTODOS:



Ggplot1 = ggplot() + 
  geom_point(data = data, aes(x = x_dado, y = y_dado), col = "black", pch=4) + 
  geom_line(data = dataCS, mapping = aes(x = x, y = CS), col = "gold")  +
  geom_line(data = dataDN, mapping = aes(x = x, y = DN), col = "blue")  +
  geom_point(data = data,aes(x =0.0522 , y =  InterpolacionCubicoNatural(x, x_dado, y_dado)) , col = "gold", pch=7) +
  geom_point(data = data,aes(x =0.0522 , y =  pnorm(0.0522,mean = 1.092, sd = 1.044)) , col = "blue", pch=9) 

Ggplot1


#Ahi se puede notar como la interpolación de Cubic Spline es mejor que Ajustamiento.
#Además se puede ver como la interpolación pasa por todos los puntos, aunque en algunos lugares
#se aleja de los mismos. En cambio, el de ajustamiento, en este caso en particula, pasa por varios puntos
#pero no se acerca al que estoy buscando. Se ve que está màs cerca del 0.0817.

#Agrego el de interpolación de Newton:

Ggplot2 = Ggplot1 +  
  geom_line(data = dataP, mapping = aes(x = x, y = P), col="orange") +
  geom_point(data = data,aes(x =0.0522 , y =  Interp_Lagrange(x, x_dado, y_dado)) , col = "orange", pch=5) +
   
Ggplot2


