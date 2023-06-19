# 1. Integracion Numerica ---- 
ReglaCompuestaSimpson<-function(a,b,n,f){
  h=(b-a)/n
  I0=f(a)+f(b)
  I1=0
  I2=0
  x=0
  for(i in 1:(n-1)){
    x=a+i*h
    if(i%%2==0){
      I2=I2+f(x)
    }else{
      I1=I1+f(x)
    }
  }
  return(h*(I0+2*I2+4*I1)/3)
}

ReglaCompuestaTrapezoidal<-function(a,b,n,f){
  h=(b-a)/n
  I1=0
  for(i in 1:(n-1)){
    I1=I1+f(a+i*h)
  }
  return(h/2*(f(a)+2*I1+f(b)))
}


r = function(t){(t^(9.22-1))*exp(-t)}

# 1. a ----
ReglaCompuestaSimpson(0, 1000, 100, r)
# 101141.9

# 1. b ----
alpha = 9.22
beta = 2
  
f = function(x){(r(alpha+beta)/r(alpha)*r(beta))*x^(alpha - 1)*(1-x)^(beta-1)}

ReglaCompuestaTrapezoidal(0, 0.6, 50, f)
# 0.01231072

# Cota de error:
alpha = 9.22
beta = 2
gamma = function(t){(t^(9.22-1))*exp(-t)}
funcion = expression((gamma(alpha+beta)/gamma(alpha)*gamma(beta))*x^(alpha - 1)*(1-x)^(beta-1))

x  =  seq(0, 0.6, length.out = 10000)
fx = eval(D(D(funcion, "x"),"x"), list(x = x))

limitesuperior = 0.6
limiteinferior = 0
n = 50
h = (limitesuperior - limiteinferior)/n

# Trapecio compuesta
((limitesuperior - limiteinferior)/12)*(h^2)* max(abs(fx))
# 0.0001142403

# 2. Simulacion ---- 
P0 = 75
mu = 0.15
sigma = 0.20
T = 0.5
n = 182 # Se redondea para abajo como lo visto en clase
dt = T/n

# a. Simulacion de caminos e historgrama de precio final ----
m = 1000  
Pt = matrix(NA, nrow = m, ncol = n+1) 

Pt[,1] = P0

for (i in 1:m) {
  for (t in 2:(n+1)){
    Pt[i,t] = Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
  } 
} 

hist(Pt[,183])

# b. Probabilidad precio Final ----
library(tidyverse)
df_Pt = as.tibble(Pt[,183])

a = df_Pt %>% filter(value > 65) %>%  filter(value < P0)

nrow(a) / nrow(df_Pt)
# 0.247
# Tambien lo podria hacer directamente simulando el precio final PT y sobre ese sacar la probabilidad
m = 1000
e = rnorm(m) 
PT = matrix(NA,nrow = m, ncol = 1) 
PT = P0 * exp((mu - 0.5*sigma^2)*T + sigma * sqrt(T) * e) 
df_PT = as.tibble(PT)

a = df_PT %>% filter(value > 65) %>%  filter(value < P0)

nrow(a) / nrow(df_PT)
# 0.251
# Si bien no da exactamente igual, si realizaramos un mayor numero de simulaciones
# Este numero se aproximaria aun mas 



# c. Probabilidad precio final - esperado ----
# Nuevamente este punto se puede hacer tomando la ultima columna por el camino de precios
# O directamente tomando el precio final. 

mean(PT) # 81.22521
mean(Pt[,183]) # 80.86516

EPT = mean(PT)
EPt = mean(Pt[,183])

df_PT = as.tibble(PT)
a = df_PT %>% filter(value < EPT)
nrow(a) / nrow(df_PT)

df_Pt = as.tibble(Pt[,183])
a = df_Pt %>% filter(value < EPt)
nrow(a) / nrow(df_Pt)

# 0.539 Usando el precio final
# 0.537 Usando el camino de precios

# Nota: Al no haber plantado semilla los resultados pueden variar al volver a correrlo  

# 3. Interpolacion ----
# a. Teorica ----
# La interpolacion de Newton requiere calcular las diferencias divididas, 
# lo que no es necesaria en la interpolacion de Lagrange. Esto es conveniente
# cuando buscamos polinomios de grado alto. 
# b. Interpolacion (aclaracion clase. Con 3 puntos esta bien) ----

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

x_dado = c(13.4483, 14.1008, 14.4795)
y_dado = c(0.6, 0.7, 0.8)

Interp_Lagrange(14, x_dado, y_dado)
# 0.6785762

x_dado2 = c(14.4795, 15.2031, 19.6600)
y_dado2 = c(0.8, 0.9, 1)

Interp_Lagrange(18, x_dado2, y_dado2)
# 1.066501 Esto se debe a que Lagrange tiene dificultades para calcular
# los puntos extremos. De realizarlo con dos puntos seguramente
# nos daria un mejor valor, ya que tendriamos una recta por lo que
# esto impide que tome valores muy altos en los extremos por el comportamiendo de la función.

x_dado3 = c(15.2031, 19.6600)
y_dado3 = c(0.9, 1)

Interp_Lagrange(18, x_dado3, y_dado3)
# 0.9627544
