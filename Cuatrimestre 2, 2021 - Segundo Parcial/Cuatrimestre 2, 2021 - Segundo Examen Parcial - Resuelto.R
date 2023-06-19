#2do Parcial - 2021, 2Q

#EJERCICIO 1 - Integrales

# Métodos -----------------------------------------------------------------

Trapecio <- function(limiteInferior, limiteSuperior, funcion, n){
  #browser()
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  # Hay que cambiarlo para que quede solo con un 1
  if (n == 1){
    return((h/2) * (fx[1] + fx[2]))
  }
  
}

Simpson <- function(limiteInferior, limiteSuperior, funcion, n){
  #browser()
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  if (n == 2){
    return((h/3) * (fx[1] + 4*fx[2] + fx[3]))
  }
  
}

Simpson38 <- function(limiteInferior, limiteSuperior, funcion, n){
  h <- (limiteSuperior - limiteInferior)/n
  
  fx <- rep(NA, times = (n+1))
  for (i in 1:(n+1)) {
    fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
  }
  
  if(n == 3){
    return((3/8)*h*(fx[1] + 3*fx[2] + 3*fx[3] + fx[4]))
  }
  
}

TrapecioCompuesta <- function(limiteInferior, limiteSuperior, funcion, n, cantIntervalos){
  
  #browser()
  if ((n == 2 || n == 0) && cantIntervalos%%2 != 0){
    return("cantIntervalos debe ser un entero par")
  }
  
  cantIntervalos <- cantIntervalos/n
  
  
  crecimientoIntervalo <- (limiteSuperior-limiteInferior)/cantIntervalos
  
  fx <- rep(NA, times = (n+1))
  
  resultado <- 0
  
  for (i in 1:cantIntervalos) {
    limiteSuperior <- limiteInferior + crecimientoIntervalo
    
    if (n != 0){
      h <- (limiteSuperior - limiteInferior)/n
    }
    
    
    for (i in 1:(n+1)) {
      fx[i] <- eval(funcion, list(x = limiteInferior + (i-1)*h))
    }
    
    # Trapecio
    if (n == 1){
      resultado <- resultado + (h/2) * (fx[1] + fx[2])
    }
    
    limiteInferior <- limiteSuperior
  }
  
  return(resultado)
}



# 1. Integracion ----------------------------------------------------------
alfa <- 4.36
theeta <- 2.52
funcion <- expression( (alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1)) )

# 1.1 Probabilidades simple -----------------------------------------------
# Trapecio
Trapecio(limiteInferior = 1.15, limiteSuperior = 4.99, funcion = funcion, n = 1)
# Nodos y0, y1

# Simpson
Simpson(limiteInferior = 1.15, limiteSuperior = 4.99, funcion = funcion, n = 2)
# Nodos y0, y1, y2

# Simpson 3/8
Simpson38(limiteInferior = 1.15, limiteSuperior = 4.99, funcion = funcion, n = 3)
# Nodos y0, y1, y2, y3


# 1.2 Probabilidades compuesto --------------------------------------------

TrapecioCompuesta(limiteInferior = 1.15, limiteSuperior = 4.99, funcion = funcion, n = 1, cantIntervalos = 17)
# Nodos y0, y1

# Cota del error
x <- seq(1.15, 4.99, length.out = 10000)

fx <- eval(D(D(funcion, "x"),"x"), list(x = x))


limiteSuperior <- 4.99
limiteInferior <- 1.15
n <- 17

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx))

# El resultado es mas exacto porque trapecio compuesto genera cierta cantidad de intervalos, en este caso 17,
# Y genera un trapecio por intervalo, por consecuencia, la cota del error es menor.


# 1.3 Esperanza -----------------------------------------------------------



e1 <- TrapecioCompuesta(limiteInferior = 10^-10, limiteSuperior = 100, funcion = expression( x * ((alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1))) ), n = 1, cantIntervalos = 474)
e1

# Cota del error
x <- seq(10^-10, 100, length.out = 10000)

fx <- eval(D(D(expression( x * ((alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1))) ), "x"),"x"), list(x = x))


limiteSuperior <- 10^-10
limiteInferior <- 100
n <- 474

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx))


# 1.4 Varianza ------------------------------------------------------------

# E[Y^2]
e2 <- TrapecioCompuesta(limiteInferior = 10^-10, limiteSuperior = 100, funcion = expression( (x^2) * ((alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1))) ), n = 1, cantIntervalos = 474)

e2 - e1^2

# Cota del error
x <- seq(10^-10, 100, length.out = 10000)

fx <- eval(D(D(expression( (x^2) * ((alfa^2 * (x/theeta)^alfa)/(x*(1+(x/theeta)^alfa)^(alfa+1))) ), "x"),"x"), list(x = x))


limiteSuperior <- 10^-10
limiteInferior <- 100
n <- 474

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx))















#EJERCICIO 2 - Derivadas--------------------------------------------

TresPuntos_extremos <- function(fx, h) {
  return((1/(2*h))*(-3*fx[1]+4*fx[2]-fx[3]))
}

SegundaDerivada <- function (x, fx){
  n <- length(x)
  
  fprima <- rep(NA, times = n)
  
  h <- x[2] - x[1]
  
  #Punto medio
  for (i in 2:(n-1)) {
    fprima[i] <- (1/(h^2))*(fx[i-1]-2*fx[i]+fx[i+1])
  }
  
  tabla <- data.frame(x, fx, fprima)
  
  return(tabla)
  
}


# Ejercicio 2. Derivación -------------------------------

# Cargo un df con los datos
df <- data.frame(r = seq(from = 0, to = 0.12, by = 0.01),
                 P = c(115.3371, 109.7031, 104.8099, 100.0653,  95.3516,  91.1998,  87.2676,  83.5825,  79.9116,  76.8435,  73.9473,
                       70.0320, 67.7022))


# 2.1 Derivada primera ----------------------------------------------------
# P'(0.03)
TresPuntos_extremos(fx = c(100.0653, 95.3516, 91.1998), h = 0.01)

# P'(0.04)
TresPuntos_extremos(fx = c(95.3516, 91.1998, 87.2676), h = 0.01)


# Derivada segunda --------------------------------------------------------
derivadas <- print(SegundaDerivada(x = df$r, fx = df$P))

print(paste("P''(0.04) =", 5619))
print(paste("P''(0.03) =", 309))













#EJERCICIO 3 - Interpolacion y Ajustamiento 

# Librerias ---------------------------------------------------------------

library(ggplot2)

# Métodos -----------------------------------------------------------------

PolinomioLagrange <- function(x, fx, y){
  
  n <- length(x)
  
  l <- rep("", times = n)
  
  resultado <- 0
  
  for (i in 1:n) {
    l[i] <- fx[i]
    for (j in 1:n) {
      if (j != i){
        l[i] <- l[i] + glue::glue("*(x-",x[j],")/(",x[i],"-",x[j],")")
      }
    }
  }
  
  for(i in 1:n){
    resultado <- resultado + eval(parse(text=l[i]), y)
  }
  return(paste("El resultado es: ", resultado))
}

SplineNatural <- function(x, y){
  #browser()
  n <- length(x)
  
  # Paso 1
  h <- rep(NA, times = (n-1))
  for (i in 1:(n-1)) {
    h[i] <- x[i+1] - x[i]
  }; rm(i)
  
  # Paso 2
  alfa <- rep(NA, times = (n-2))
  for (i in 2:(n-1)) {
    alfa[i] <- (3/h[i]) * (y[i+1] - y[i]) - (3/h[i-1]) * (y[i] - y[i-1])
  }
  
  # Paso 3
  mu <- rep(NA, times = n)
  zeta <- rep(NA, times = n)
  l <- rep(NA, times = n)
  
  mu[1] <- 0
  zeta[1] <- 0
  l[1] <- 1
  
  
  # Paso 4
  for (i in 2:(n-1)) {
    l[i] <- 2 * (x[i+1] - x[i-1]) - h[i-1] * mu[i-1]
    mu[i] <- h[i]/l[i]
    zeta[i] <- (alfa[i] - h[i-1] * zeta[i-1])/l[i]
  }
  
  # Paso 5
  l[n] <- 1
  zeta[n] <- 0
  c <- rep(NA, times = n)
  c[n] <- 0
  
  # Paso 6
  b <- rep(NA, times = (n-1))
  d <- rep(NA, times = (n-1))
  for (j in (n-1):1) {
    c[j] <- zeta[j] - mu[j] * c[j+1]
    b[j] <- (y[j+1] - y[j]) / h[j] - h[j] * (c[j+1] + 2 * c[j])/3
    d[j] <- (c[j+1] - c[j]) / (3*h[j])
  }
  
  #Paso 7
  resultados <- matrix(rep(NA, 4*(n-1)), nrow = (n-1), ncol = 4, byrow = F)
  for (k in 1:(n-1)) {
    resultados[k, 1] <- y[k]
    resultados[k, 2] <- b[k]
    resultados[k, 3] <- c[k]
    resultados[k, 4] <- d[k]
  }
  
  #print(resultados)
  
  #Construyo el polinomio
  polinomios <- rep(NA, times = nrow(resultados))
  for (i in 1:nrow(resultados)) {
    polinomios[i] <- glue::glue(resultados[i,1]) 
    for(j in 2:ncol(resultados)){
      polinomios[i] <- polinomios[i] + glue::glue(" + ", resultados[i,j], " * (x - ", x[i], ")^", (j-1)) 
    }
  }
  
  return(polinomios)
}

DiferenciasDivididas <- function(x, y){
  n <- length(x)
  
  #Hago un vector vacio para llenar el df
  empty_vec <- rep(0, times = n)  
  
  df <- data.frame(x, y)
  
  for (i in 1:(n-1)) {
    
    df[glue::glue("Q",i)] <- empty_vec
    
    for (j in (i+1):n) {
      
      df[j, (i+2)] <- ( df[j,(i+1)] - df[(j-1),(i+1)])/(x[j]-x[j-i])
    }
  }
  
  return(df)
}

PolinomioInterpolanteNewton <- function(x, y){
  df <- DiferenciasDivididas(x = x, y = y)
  
  #Saco la primer columna del df
  df[,1] <- NULL
  
  n <- ncol(df)
  
  polinomio <- df[1,1]
  
  for (i in 2:n) {
    polinomio <- polinomio + glue::glue(" + ", df[i,i])
    for (j in 1:(i-1)) {
      polinomio <- polinomio + glue::glue(" * ( x - ", x[j], " )")
    }
  }
  return(polinomio)
}

# Ejercicio 3. Interpolación y Ajustamiento -------------------------------

# Cargo un df con los datos
df <- data.frame(x = c(1.6579,  3.8119,  4.1155,  5.1111,  6.0537,  6.2298, 11.6898, 11.7199, 12.7634),
                 y = c(0.1111, 0.2222, 0.3333, 0.4444, 0.5556, 0.6667, 0.7778, 0.8889, 1.0000))



# 3.1 Lagrange ------------------------------------------------------------

PolinomioLagrange(x = df$x, fx = df$y, y = list(x = 12.2416))

# El polinomio de Lagrange aproxima mal los valores extremos, por eso no es de extrañar el resultado.
# Esto se debe a que aproxima con un polinomio de grado n-1, en este caso un polinomio de grado 8.

PolinomioLagrange(x = c(11.6898, 11.7199, 12.7634), fx = c( 0.7778, 0.8889, 1.0000), y = list(x = 12.2416))
# En este caso es un polinomio de grado 2, pero sigue aproximando "mal" un valor extremo

PolinomioLagrange(x = c(11.7199, 12.7634), fx = c(0.8889, 1.0000), y = list(x = 12.2416))
# En el último caso aproxima mejor porque es un polinomio de grado 1, una recta.
# Esto impide que tome valores muy altos en los extremos por el comportamiendo de la función.

# Esto se observa mejor en el gráfico comparativo

# 3.2 Cubic Splines -------------------------------------------------------

polinomios <- SplineNatural(x = df$x, y = df$y)

# Imprimi una vez la función con los polinomios redondeados y despues le saque el método round() a la función
# [1.6579 ; 3.8119] "0.1111 + -0.0982 * (x - 1.6579)^1 + 0 * (x - 1.6579)^2 + 0.0323 * (x - 1.6579)^3"        
# [3.8119 ; 4.1155] "0.2222 + 0.3512 * (x - 3.8119)^1 + 0.2086 * (x - 3.8119)^2 + -0.527 * (x - 3.8119)^3"    
# [4.1155 ; 5.1111] "0.3333 + 0.3321 * (x - 4.1155)^1 + -0.2713 * (x - 4.1155)^2 + 0.05 * (x - 4.1155)^3"     
# [5.1111 ; 6.0537] "0.4444 + -0.0594 * (x - 5.1111)^1 + -0.1219 * (x - 5.1111)^2 + 0.329 * (x - 5.1111)^3"   
# [6.0537 ; 6.2298] "0.5556 + 0.5876 * (x - 6.0537)^1 + 0.8083 * (x - 6.0537)^2 + -3.1946 * (x - 6.0537)^3"   
# [6.2298 ; 11.6898] "0.6667 + 0.5751 * (x - 6.2298)^1 + -0.8794 * (x - 6.2298)^2 + 0.1425 * (x - 6.2298)^3"   
# [11.6898 ; 11.7199] "0.7778 + 3.7123 * (x - 11.6898)^1 + 1.454 * (x - 11.6898)^2 + -71.7894 * (x - 11.6898)^3"
# [11.7199 ; 12.7634] "0.8889 + 3.6047 * (x - 11.7199)^1 + -5.0286 * (x - 11.7199)^2 + 1.6063 * (x - 11.7199)^3"

# El punto esta en el intervalo [11.7199 ; 12.7634], por lo tanto, uso el polinomio asociado a ese intervalo

interploando_sp <- eval(parse(text = polinomios[8]), list(x = 12.2416))
interploando_sp
# Cubic Spline utiliza polinomios de grado 3 para aproximar en cada intervalo.
# Tiene la ventaja que da resultados mas suaves en los valores extremos que los polinomios de Lagrange


# 3.3 Ajustamiento --------------------------------------------------------


# 3.4 Gráfico comparativo -------------------------------------------------

# Genero el polinomio de Newton
polinomio_newton <- PolinomioInterpolanteNewton(x = df$x, y = df$y)

x_newton <- seq(from = 1.6579, to = 12.7634, by = 0.001)
y_newton <- eval(parse(text = polinomio_newton), list(x = x_newton))
y_interpolado_newton <- eval(parse(text = polinomio_newton), list(x = 12.2416))

# Evaluo el Spline
x_sp1 <- seq(from = 1.6579, to = 3.8119, by = 0.01)
fx_sp1 <- eval(parse(text = polinomios[1]), list(x = x_sp1))

x_sp2 <- seq(from = 3.8119, to = 4.1155, by = 0.01)
fx_sp2 <- eval(parse(text = polinomios[2]), list(x = x_sp2))

x_sp3 <- seq(from = 4.1155, to = 5.1111, by = 0.01)
fx_sp3 <- eval(parse(text = polinomios[3]), list(x = x_sp3))

x_sp4 <- seq(from = 5.1111, to = 6.0537, by = 0.01)
fx_sp4 <- eval(parse(text = polinomios[4]), list(x = x_sp4))

x_sp5 <- seq(from = 6.0537, to = 6.2298 , by = 0.01)
fx_sp5 <- eval(parse(text = polinomios[5]), list(x = x_sp5))

x_sp6 <- seq(from = 6.2298 , to = 11.6898, by = 0.01)
fx_sp6 <- eval(parse(text = polinomios[6]), list(x = x_sp6))

x_sp7 <- seq(from = 11.6898, to = 11.7199 , by = 0.01)
fx_sp7 <- eval(parse(text = polinomios[7]), list(x = x_sp7))

x_sp8 <- seq(from = 11.7199 , to = 12.7634 , by = 0.01)
fx_sp8 <- eval(parse(text = polinomios[8]), list(x = x_sp8))

ggplot() +
  geom_point(aes(x = df$x, y = df$y), colour = "green", shape = 17) +
  geom_line(aes(x = x_newton, y = y_newton), colour = "pink") +
  geom_point(aes(x = 12.2416, y_interpolado_newton), colour = "pink", shape = 15) +
  geom_line(aes(x = x_sp1, y = fx_sp1), colour = 'blue') +
  geom_line(aes(x = x_sp2, y = fx_sp2), colour = 'blue') +
  geom_line(aes(x = x_sp3, y = fx_sp3), colour = 'blue') +
  geom_line(aes(x = x_sp4, y = fx_sp4), colour = 'blue') +
  geom_line(aes(x = x_sp5, y = fx_sp5), colour = 'blue') +
  geom_line(aes(x = x_sp6, y = fx_sp6), colour = 'blue') +
  geom_line(aes(x = x_sp7, y = fx_sp7), colour = 'blue') +
  geom_line(aes(x = x_sp8, y = fx_sp8), colour = 'blue') +
  geom_point(aes(x = 12.2416, interploando_sp), colour = "blue", shape = 10) +
  xlab("x") + ylab("F(x) = P(L<x)") 


















#EJERCICIO 4 - SIMULACION 

# Librerias ---------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

# Ejercicio 4.1 Camino de precios -----------------------------------------
set.seed(895700)
p0 <- 73
mu <- 0.13
sigma <- 0.1

anios <- 0.5

# Simulacion
m <- 1233

# Time steps
n <- 182.5

dt <- anios/n

# Matriz de camino de precios
P0aT <- matrix(NA, nrow = m, ncol = n+1)

P0aT[,1] <- p0

for (i in 1:m) {
  for(t in 2:(n+1)){
    P0aT[i,t] <- P0aT[i, t-1] * exp((mu-0.5*sigma^2) * dt + sigma * sqrt(dt) * rnorm(1))
  }
}

ggplot() +
  geom_histogram(aes(P0aT[,ncol(P0aT)]), binwidth = 5) +
  xlab("Price") + ylab("Times")

mean(P0aT[,ncol(P0aT)])
sd(P0aT[,ncol(P0aT)])


# 4.2 Probabilidad 1 ------------------------------------------------------

df_p0aT <- as_tibble(P0aT[,ncol(P0aT)])

a <- df_p0aT %>% filter(value > 38) %>%  filter(value < p0)

nrow(a) / nrow(df_p0aT)

# 4.3 Probabilidad 2 ------------------------------------------------------

df_p0aT <- as.tibble(P0aT[,ncol(P0aT)])

a <- df_p0aT %>% filter(value > mean(P0aT[,ncol(P0aT)]))

nrow(a) / nrow(df_p0aT)


















#EJERCICIO 5 - Ecuaciones Diferenciales

# librerias ---------------------------------------------------------------

library(ggplot2)

# Métodos -----------------------------------------------------------------
MetodoEuler <- function(a, b, N, alfa, funcion){
  
  df <- data.frame(t = rep(NA, times = (N+1)), w = rep(NA, times = (N+1)))
  
  # Paso 1
  h <- (b - a) / N
  df[1,1] <- a
  df[1,2] <- alfa
  
  
  # Paso 2
  for (i in 1:N) {
    
    # Paso 3
    df[i+1,1] <- a + i*h
    df[i+1,2] <- df[i,2] + h * eval(funcion, list(y = df[i,2], t = (a + (i-1)*h) ))
  }
  
  return(df)
}


RungeKutta <- function(a, b, N, alfa, funcion){
  
  df <- data.frame(t = rep(NA, times = (N+1)), w = rep(NA, times = (N+1)))
  
  h <- (b - a)/N
  df[1,1] <- a
  df[1,2] <- alfa
  t <- a
  
  for (i in 1:N) {
    k <- rep(NA, times = 4)
    
    # Paso 3
    k[1] <- h * eval(funcion, list(t = t, y = df[i,2] ))
    k[2] <- h * eval(funcion, list(t = t + h/2, y = df[i,2] + k[1]/2))
    k[3] <- h * eval(funcion, list(t = t + h/2, y = df[i,2] + k[2]/2))
    k[4] <- h * eval(funcion, list(t = t + h, y = df[i,2] + k[3]))
    
    df[i+1,2] <- df[i,2] + (k[1] + 2*k[2] + 2*k[3] + k[4])/6
    
    t <- a + i * h
    df[i+1,1] <- t
  }
  
  return(df)
}

# a -----------------------------------------------------------------------
# Alfa = y(0)
a <- MetodoEuler(a = 4, b = 5, N = 45, alfa = 0.46, funcion = expression((cos(y)/t^(1.69))+t/y^3))
a

# b -----------------------------------------------------------------------
# Alfa = y(0)
b <- RungeKutta(a = 4, b = 5, N = 4, alfa = 0.46, funcion = expression((cos(y)/t^(1.69))+(t/(y^3))))


# c -----------------------------------------------------------------------

ggplot() +
  geom_line(aes(x = a$t, y = a$w), colour = "darkgreen") +
  geom_line(aes(x = b$t, y = b$w), colour = "darkblue")

# d -----------------------------------------------------------------------
# Los métodos de Taylor de orden superior (como Runge-Kutta) tienen error de truncamiento de orden alto, lo que hace que aproximen mejor,
# pero es necesario conocer las derivadas. En cambio, el método de Euler, lo hace mas "económico" porque no es necesario conocer las derivadas
# pero esto lo hace menos exacto.

