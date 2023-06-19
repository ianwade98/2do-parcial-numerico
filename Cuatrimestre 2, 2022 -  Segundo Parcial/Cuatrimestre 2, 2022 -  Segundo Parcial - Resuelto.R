#Cuatrimestre 2, 2022. Segundo Examen Parcial

## Ejercicio 1
f1 = function(x){((x/2.83)^1.42*exp(-x/2.83))/(x*gamma(1.42))}

## 1.1
trapecio = function(f, x0, xn){ # f es la función a integrar, x0 es límite inferior, xn es límite superior
  n = 1
  h = abs((x0 - xn)/n)
  fx0 = f(x0)
  fx1 = f(xn)
  integral = h/2*(fx0 + fx1)
  return(integral)
}
simpson = function(f, x0, xn){ # f es la función a integrar, x0 es límite inferior, xn es límite superior
  n = 2
  h = abs((x0 - xn)/n)
  x1 = x0 + h
  fx0 = f(x0)
  fx1 = f(x1)
  fx2 = f(xn)
  integral = h/3*(fx0 + 4*fx1 + fx2)
  return(integral)
}
simpson38 = function(f, x0, xn){ # f es la función a integrar, x0 es límite inferior, xn es límite superior
  n = 3
  h = abs((x0 - xn)/n)
  x1 = x0 + h
  x2 = x1 + h
  fx0 = f(x0)
  fx1 = f(x1)
  fx2 = f(x2)
  fx3 = f(xn)
  integral = (3/8)*h*(fx0 + 3*fx1 + 3*fx2 + fx3)
  return(integral)
}

trapecio(f1, 44.13, 49.89)
nodos_1.1.a = c(44.13, 49.89)
nodos_1.1.a

simpson(f1, 44.13, 49.89)
nodos_1.1.b = c(44.13, 44.13 + (49.89 - 44.13)/2, 49.89)
nodos_1.1.b

simpson38(f1, 44.13, 49.89)
nodos_1.1.c = c(44.13, 44.13 + (49.89 - 44.13)/3, 44.13 + 2*(49.89 - 44.13)/3, 49.89)
nodos_1.1.c

# 1.2
comp_trapezoidal = function(f, a, b, n){ # f es la función a integrar, a es límite inferior, b es límite superior, n puede ser impar
  # Paso 1
  h = abs((a - b)/n)
  # Paso 2
  XI0 = f(a) + f(b)
  XI1 = 0
  # Paso 3
  for (i in 1:(n-1)){
    # Paso 4
    X = a + i*h
    # Paso 5
    XI1 = XI1 + f(X)
  }
  # Paso 6
  XI = h/2*(XI0 + 2*XI1)
  # Paso 7
  return(XI)
}

comp_trapezoidal(f1, 44.13, 48.89, 25)

# Nodos utilizados
nodos_1.2 = rep(NA, 26)
h.2 = (48.89 - 44.13)/25
for (i in 1:26){
  nodos_1.2[i] = 44.13 + (i - 1)*h.2
}
nodos_1.2

# Cota del error
x.2 = seq(44.13, 48.89, 10^-3)
DD <- function(expr, name, order = 1) { # función copiada de ?D
  if(order < 1) stop("'order' must be >= 1")
  if(order == 1) D(expr, name)
  else DD(D(expr, name), name, order - 1)
}
fx.2 = eval(DD(expression(((x/2.83)^1.42*exp(-x/2.83))/x*gamma(1.42)), 'x', 2), list(x = x.2))
error.2 = -(48.89 - 44.13)/12*h.2^2*max(fx.2)
error.2

# 1.3
E = function(x){((x/2.83)^1.42*exp(-x/2.83))/gamma(1.42)} # E(x) = integral de x*f(x)dx
E.Y = comp_trapezoidal(E, 0, 50, 320)
E.Y

# Cota de error
x.3 = seq(0, 50, 10^-3)
h.3 = (50 - 0)/320
fx.3 = eval(DD(expression(((x/2.83)^1.42*exp(-x/2.83))/gamma(1.42)), 'x', 2), list(x = x.3))
error.3 = -(50 - 0)/12*h.3^2*max(fx.3)
error.3

# 1.4
comp_simpson = function(f, a, b, n){ # f es la función a integrar, a es límite inferior, b es límite superior, n es entero par
  # Paso 1
  h = abs((a - b)/n)
  # Paso 2
  XI0 = f(a) + f(b)
  XI1 = 0
  XI2 = 0
  # Paso 3
  for (i in 1:(n-1)){
    # Paso 4
    X = a + i*h
    # Paso 5
    if (i%%2 == 0){
      XI2 = XI2 + f(X)
    }
    if (i%%2 != 0){
      XI1 = XI1 + f(X)
    }
  }
  # Paso 6
  XI = h/3*(XI0 + 2*XI2 + 4*XI1)
  # Paso 7
  return(XI)
}

V = function(x){
  (x - E.Y)^2*((x/2.83)^1.42*exp(-x/2.83))/(x*gamma(1.42))
}
V.Y = comp_simpson(V,10^-7,50,320) # usé 10^-7 en lugar de 0, ya que daba NaN
V.Y

# Cota de error
x.4 = seq(10^-3, 50, 10^-3)
fx.4 = eval(DD(expression((x - E.Y)^2*((x/2.83)^1.42*exp(-x/2.83))/(x*gamma(1.42))), 'x', 4), list(x = x.4))
error.4 = -(50-10^-3)/180*h.3^4*max(fx.4)
error.4

## Ejercicio 2
tabla_deriv = matrix(c(30,32.5,35,37.5,40,42.5,45,47.5,50,.0914,.3309,.8826,1.8661,3.3111,5.1551,7.2878,9.6009,12.0135), ncol = 2)

# 2.1
cincop_ext = function(x_dado, fx_dado, x0, h){ # x0 es el punto donde se aproxima
  n = length(x_dado)
  for (i in 1:n){
    if (x_dado[i] == x0){
      if (h < 0){ # si x0 es el límite superior
        der = (1/(12*h))*(-25*fx_dado[i] + 48*fx_dado[(i-1)] - 36*fx_dado[(i-2)] + 16*fx_dado[(i-3)] - 3*fx_dado[(i-4)])
      } # en caso que abs(h) == x1 - x0
      if (h > 0){ # si x0 es el límite inferior
        der = (1/(12*h))*(-25*fx_dado[i] + 48*fx_dado[(i+1)] - 36*fx_dado[(i+2)] + 16*fx_dado[(i+3)] - 3*fx_dado[(i+4)])
      } # en caso que abs(h) == x1 - x0
    }
  }
  return(der)
}

c40 = cincop_ext(tabla_deriv[,1], tabla_deriv[,2], 40, 2.5)
c40
# no se puede aproximar c'(45) ya que tienen que haber más datos, (i+3) e (i+4) son faltantes, o con h<0 se puede también hallar la derivada

# 2.2
segp_medio = function(x_dado, fx_dado, x0, h){ # x0 es el punto donde se aproxima
  n = length(x_dado)
  for (i in 1:n){
    if (x_dado[i] == x0){
      der = h^-2*(fx_dado[(i-1)] - 2*fx_dado[i] + fx_dado[(i+1)])
    } # en caso que abs(h) == x1 - x0
  }
  return(der)
}

c2_40 = segp_medio(tabla_deriv[,1], tabla_deriv[,2], 40, 2.5)
c2_40
c2_45 = segp_medio(tabla_deriv[,1], tabla_deriv[,2], 45, 2.5)
c2_45

## Ejercicio 3
# 3.1
interpolacion_lagrange = function(x, x_dado, fx_dado){
  n = length(x_dado)
  l = rep(NA, n)
  for (k in 1:n){
    l[k] = fx_dado[k]
    for (i in 1:n){
      if (k != i){
        l[k] = l[k]*(x-x_dado[i])/(x_dado[k]-x_dado[i])
      }
    }
  }
  px = 0
  for (k in 1:n){
    px = px + l[k]
  }
  return(px)
}

interpolacion_lagrange(48.75, tabla_deriv[,1], tabla_deriv[,2])

interpolacion_lagrange(48.75, tabla_deriv[6:9,1], tabla_deriv[6:9,2])

interpolacion_lagrange(48.75, tabla_deriv[8:9,1], tabla_deriv[8:9,2])

# 3.2
splinenatural = function(x_dado, fx_dado){
  x = x_dado
  a = fx_dado
  n = length(x_dado)
  ## Paso 1
  h = rep(NA, times = (n-1))
  for (i in 1:(n-1)){
    h[i] = x[(i+1)] - x[i]
  }
  ## Paso 2
  alfa = rep(NA, times = (n-2))
  for (i in 2:(n-1)){
    alfa[i] = 3/h[i]*(a[(i+1)] - a[i]) - 3/h[(i-1)]*(a[i] - a[(i-1)])
  }
  ## Paso 3
  l = rep(NA, times = n)
  mu = rep(NA, times = n)
  z = rep(NA, times = n)
  l[1] = 1
  mu[1] = 0
  z[1] = 0
  ## Paso 4
  for (i in 2:(n-1)){
    l[i] = 2*(x[(i+1)] - x[(i-1)]) - h[(i-1)]*mu[(i-1)]
    mu[i] = h[i]/l[i]
    z[i] = (alfa[i] - h[(i-1)]*z[(i-1)])/l[i]
  }
  ## Paso 5
  l[n] = 1
  z[n] = 0
  c = rep(NA, times = n)
  c[n] = 0
  ## Paso 6
  b = rep(NA, times = n)
  d = rep(NA, times = n)
  for (j in (n-1):1){
    c[j] = z[j] - mu[j]*c[(j+1)]
    b[j] = (a[(j+1)] - a[j])/h[j] - h[j]*(c[(j+1)] + 2*c[j])/3
    d[j] = (c[(j+1)] - c[j])/(3*h[j])
  }
  ## Paso 7
  constantes = matrix(rep(NA, times = 4*(n-1)), nrow = (n-1))
  for (j in 1:(n-1)){
    constantes[j, 1] = a[j]
    constantes[j, 2] = b[j]
    constantes[j, 3] = c[j]
    constantes[j, 4] = d[j]
  }
  ## Armo los polinomios
  polinomios = rep(NA, times = (n-1))
  for (i in 1:(n-1)){
    polinomios[i] = glue::glue(round(constantes[i, 1]), 4)
    for (j in 2:4){
      if (constantes[i, j] != 0){
        polinomios[i] = polinomios[i] + glue::glue(" + ", round(constantes[i, j], 4), " * (x - ", x[i], ")^", (j-1))
      }
    }
  }
  return(polinomios)
}

trazador.cubico = splinenatural(tabla_deriv[,1], tabla_deriv[,2])
trazador.cubico

# Función para obtener el resultado
splinenatural.noround = function(x_dado, fx_dado){
  x = x_dado
  a = fx_dado
  n = length(x_dado)
  ## Paso 1
  h = rep(NA, times = (n-1))
  for (i in 1:(n-1)){
    h[i] = x[(i+1)] - x[i]
  }
  ## Paso 2
  alfa = rep(NA, times = (n-2))
  for (i in 2:(n-1)){
    alfa[i] = 3/h[i]*(a[(i+1)] - a[i]) - 3/h[(i-1)]*(a[i] - a[(i-1)])
  }
  ## Paso 3
  l = rep(NA, times = n)
  mu = rep(NA, times = n)
  z = rep(NA, times = n)
  l[1] = 1
  mu[1] = 0
  z[1] = 0
  ## Paso 4
  for (i in 2:(n-1)){
    l[i] = 2*(x[(i+1)] - x[(i-1)]) - h[(i-1)]*mu[(i-1)]
    mu[i] = h[i]/l[i]
    z[i] = (alfa[i] - h[(i-1)]*z[(i-1)])/l[i]
  }
  ## Paso 5
  l[n] = 1
  z[n] = 0
  c = rep(NA, times = n)
  c[n] = 0
  ## Paso 6
  b = rep(NA, times = n)
  d = rep(NA, times = n)
  for (j in (n-1):1){
    c[j] = z[j] - mu[j]*c[(j+1)]
    b[j] = (a[(j+1)] - a[j])/h[j] - h[j]*(c[(j+1)] + 2*c[j])/3
    d[j] = (c[(j+1)] - c[j])/(3*h[j])
  }
  ## Paso 7
  constantes = matrix(rep(NA, times = 4*(n-1)), nrow = (n-1))
  for (j in 1:(n-1)){
    constantes[j, 1] = a[j]
    constantes[j, 2] = b[j]
    constantes[j, 3] = c[j]
    constantes[j, 4] = d[j]
  }
  ## Armo los polinomios
  polinomios = rep(NA, times = (n-1))
  for (i in 1:(n-1)){
    polinomios[i] = glue::glue(constantes[i, 1])
    for (j in 2:4){
      if (constantes[i, j] != 0){
        polinomios[i] = polinomios[i] + glue::glue(" + ", constantes[i, j], " * (x - ", x[i], ")^", (j-1))
      }
    }
  }
  return(polinomios)
}

eval(parse(text = paste('interp_spline = function(x){
                        return(' , splinenatural.noround(tabla_deriv[,1], tabla_deriv[,2]) , ')}', sep='')))
interp_spline(48.75)

## Ejercicio 4
# 4.1
# Parámetros
p0 = 84
mu = .12
sigma = .13
T = 7/12
n = 213 # time-steps, 7/12*365 redondeado
dt = T/n

# Simulación
set.seed(904634)
m = 1867 # caminos de precios diarios
Pt = matrix(NA, nrow = m, ncol = (n+1)) # matriz de caminos de precios
Pt[,1] = p0 # en la primera columna va el precio inicial

for (i in 1:m){ # bucle que cuenta el número de simulación
  for (t in 2:(n+1)){ # bucle que barre el tiempo y va generando un camino de precios
    # arranca por la segunda columna, ya que la primera contiene a p0
    Pt[i, t] = Pt[i, (t-1)]*exp((mu - .5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
  }
}

E.Pt = mean(Pt[,n])
E.Pt
DE.Pt = sd(Pt[,n])
DE.Pt
Hist.Pt = hist(Pt[,n], main = 'Histograma de precios finales')

# 4.2
casos.p1 = 0
for (i in 1:m){
  if (Pt[i, (n+1)] > 52 & Pt[i, (n+1)] < p0){
    casos.p1 = casos.p1 + 1
  }
}
P.1 = casos.p1/m
print(paste('La probabilidad de que el precio final se halle entre 52 y 84 es de', round(P.1, 5)))

# 4.3
casos.p2 = 0
for (i in 1:m){
  if (Pt[i, (n+1)] > E.Pt){
    casos.p2 = casos.p2 + 1
  }
}
P.2 = casos.p2/m
print(paste('La probabilidad de que el precio final sea mayor que el precio esperado en T es de', round(P.2, 5)))

## Ejercicio 5
f = function(w,t){
  cos(w)*t^1.66 + t/w^3
}

# a
euler = function(a, b, alfa, N, f){
  # Paso 1
  h = (abs(a-b))/N
  t = matrix(seq(a, b, h), ncol = 1)
  w = matrix(rep(NA, (N+1)), ncol = 1)
  w[1] = alfa
  # Paso 2
  for (i in 2:(N+1)){
    # Paso 3
    w[i] = w[(i-1)] + h*f(t[(i-1)], w[(i-1)])
  }
  # Paso 4
  df = data.frame(cbind(t,w))
  colnames(df) = c('t', 'w')
  return(df)
}

Y.euler = euler(3, 4, 4.55, 11, f)
Y.euler

# b
rungekutta = function(a, b, alfa, N, f){ # f es expression
  # Paso 1
  h = (abs(a-b))/N
  t = matrix(seq(a, b, h), ncol = 1)
  w = matrix(rep(NA, (N+1)), ncol = 1)
  w[1] = alfa
  # Paso 2
  for (i in 1:N){
    # Paso 3
    k = rep(NA, 4)
    k[1] = h*f(t = t[i], w = w[i])
    k[2] = h*f(t = t[i] + h/2, w = w[i] + k[1]/2)
    k[3] = h*f(t = t[i] + h/2, w = w[i] + k[2]/2)
    k[4] = h*f(t = t[(i+1)], w = w[i] + k[3])
    # Paso 4
    w[(i+1)] = w[i] + (k[1] + 2*k[2] + 2*k[3] + k[4])/6
  }
  # Paso 5
  df = data.frame(cbind(t,w))
  colnames(df) = c('t', 'w')
  return(df)
}

Y.rg = rungekutta(3, 4, 4.55, 11, f)
Y.rg

# c
library(ggplot2)
df.5 = data.frame(rbind(Y.euler, Y.rg))
for (i in 1:24){
  if (i <= 12){
    df.5[i, 3] = 'Euler'
  }
  else {
    df.5[i, 3] = 'Runge-Kutta'
  }
}
colnames(df.5) = c('t', 'w', 'Método')
grafico.5 = ggplot(data = Y.euler) + geom_point(data = df.5, aes(x = t, y = w, color = Método)) + geom_line(data = Y.euler, aes(x = t, y = w), colour = 'firebrick2')
grafico.5 = grafico.5 + geom_line(data = Y.rg, aes(x = t, y = w), colour = 'darkslategray3')
grafico.5 = grafico.5 + ylab('y(t)') + xlab('t') + ggtitle('Comparación entre métodos de Euler y Runge-Kutta')
grafico.5
