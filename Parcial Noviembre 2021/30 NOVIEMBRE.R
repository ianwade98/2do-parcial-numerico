# PARCIAL 30 DE NOVIEMBRE 

# 3 ----
# 3. 1 ----
#Coeficientes del polinomio
Coef_Lagrange = function (x, x_dado){
  n = length(x_dado)
  L = c(rep(1, n))
  for(k in 1:n){
    for(i in 1:n){
      if( k != i){
        L[k] = L[k] * ((x - x_dado[i]) / (x_dado[k] - x_dado[i]))
      }
    }
  }
  return(L)
}
#Interpolación (aproximación)
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

# 3. 1. a.
x = 12.2416
x_dado = c(1.6579, 3.8119, 4.1155, 5.1111, 6.0537, 6.2298, 11.6898, 11.7199, 12.7634)
y_dado = c(0.1111, 0.2222, 0.3333, 0.4444, 0.5556, 0.6667, 0.7778, 0.8889, 1)

Interp_Lagrange(x, x_dado, y_dado)
# 2.768489 Polinomio de grado 8
# Con los datos dados, este es el polinomio de mayor grado que podremos obtener. 
# Para el X0 dado, siempre debremos incluir los ultimos dos X terminos, ya que de no tener
# ultimo, no tendriamos el max intervalo, y no estariamos en una interpolacion
# Finalmente podemos mencionar que el valor obtenido, no parece ser muy exacto y esto se debe a que la interpolacion
# de Lagrange tiene problemas para encontrar valores extremos. 

# 3. 1. b

x = 12.2416
x_dado = c(11.6898, 11.7199, 12.7634)
y_dado = c(0.7778, 0.8889, 1)

Interp_Lagrange(x, x_dado, y_dado)
# 1.85335
# En este caso obtenemos un polinomio de grado 2, pero la aproximacion sigue sin 
# ser muy buena dado que se trata de un valor extremo

# 3. 1. c

x = 12.2416
x_dado = c(11.7199, 12.7634)
y_dado = c(0.8889, 1)

Interp_Lagrange(x, x_dado, y_dado)
# 0.9444447
# El polinomio de grado 1 es el menos que podremos obtener y la mejor aproximacion.
# Al ser una recta, impide que tome valores muy alto en los extremos por comportamiento de la funcion. 



# 3.2 ----
options(scipen = 100, digits= 5)
Natural_CS_Polinomios = function(A){
  #Variables iniciales 
  x = A[,1]
  fx = A[,2]
  if(length(x)!=length(fx)){
    return("La cantidad de argumentos no coincide")
  }
  n = length(A[,1]); b = c(rep(NA,n)); c = c(rep(NA,n)); d = c(rep(NA,n)); h = c(rep(NA,n))
  a = c(rep(NA,n)); l = c(rep(NA,n)); u = c(rep(NA,n)); z = c(rep(NA,n))
  #Paso 1 y 2 ----
  for(i in 1:(n-1)){
    h[i] = x[i+1]-x[i]
  }
  for(i in 2:(n-1)){
    a[i] = ((3/(h[i])) * (fx[i+1]- fx[i])) - ((3/(h[i-1])) * (fx[i]- fx[i-1]))
  }
  #Paso 3 
  l[1] = 1;u[1] = 0;z[1] = 0
  #Paso 4 
  for(i in 2:(n-1)){
    l[i] = 2*(x[i+1] - x[i-1]) - (h[i-1]*u[i-1])
    u[i] = h[i]/l[i]
    z[i] = (a[i]- h[i-1]*z[i-1])/l[i] 
  }
  #Paso 5 
  l[n] = 1 ;z[n] = 0;c[n] = 0
  #Paso 6 
  for(j in (n-1):1){
    c[j] = z[j] - (u[j]*c[j+1])
    b[j] = ((fx[j+1] - fx[j])/h[j]) - h[j]*(c[j+1] + 2*c[j])/3
    d[j] = (c[j+1] - c[j])/(3*h[j])
  }
  Pol = NULL
  for(i in 1:(n-1)){
    Pol = c(Pol,paste("",fx[i],"+(",b[i],"*(x-",x[i],"))+(",c[i],"*((x-",x[i],")^2))+(",d[i],"*((x-",x[i],")^3))   para todo x perteneciente a [",x[i],",",x[i+1],"]",sep = ""))
  }
  Pol = matrix(Pol,nrow = (n-1), ncol = 1, byrow = T)
  e = NULL
  for(i in 1:(n-1)){
    e = c(e,paste("S.",i,"(x) = ",sep = ""))
  }
  dimnames(Pol) = list(e,NULL)
  return(Pol)
}

x = c(1.6579, 3.8119, 4.1155, 5.1111, 6.0537, 6.2298, 11.6898, 11.7199, 12.7634)
fx = c(0.1111, 0.2222, 0.3333, 0.4444, 0.5556, 0.6667, 0.7778, 0.8889, 1)
A=cbind(x,fx)


Natural_CS_Polinomios(A)  
round(Natural_CS_Polinomios(A), 4)

# PREGUNTAR SI ALGUIEN SABE COMO HACER LO DE LOS 4 DECIMALES

options(scipen = 100, digits= 10)
x = c(1.6579, 3.8119, 4.1155, 5.1111, 6.0537, 6.2298, 11.6898, 11.7199, 12.7634)
fx = c(0.1111, 0.2222, 0.3333, 0.4444, 0.5556, 0.6667, 0.7778, 0.8889, 1)
A=cbind(x,fx)

Interpolo_en_pto = function(A,xk){
  #Variables iniciales 
  x = A[,1]
  fx = A[,2]
  if(length(x)!=length(fx)){
    return("La cantidad de argumentos no coincide")
  }
  n = length(A[,1]); b = c(rep(NA,n)); c = c(rep(NA,n)); d = c(rep(NA,n)); h = c(rep(NA,n))
  a = c(rep(NA,n)); l = c(rep(NA,n)); u = c(rep(NA,n)); z = c(rep(NA,n))
  #Paso 1 y 2 ----
  for(i in 1:(n-1)){
    h[i] = x[i+1]-x[i]
  }
  for(i in 2:(n-1)){
    a[i] = ((3/(h[i])) * (fx[i+1]- fx[i])) - ((3/(h[i-1])) * (fx[i]- fx[i-1]))
  }
  #Paso 3 ----
  l[1] = 1;u[1] = 0;z[1] = 0
  #Paso 4 ----
  for(i in 2:(n-1)){
    l[i] = 2*(x[i+1] - x[i-1]) - (h[i-1]*u[i-1])
    u[i] = h[i]/l[i]
    z[i] = (a[i]- h[i-1]*z[i-1])/l[i] 
  }
  #Paso 5 ----
  l[n] = 1 ;z[n] = 0;c[n] = 0
  #Paso 6 ----
  for(j in (n-1):1){
    c[j] = z[j] - (u[j]*c[j+1])
    b[j] = ((fx[j+1] - fx[j])/h[j]) - h[j]*(c[j+1] + 2*c[j])/3
    d[j] = (c[j+1] - c[j])/(3*h[j])
  }
  #interpolacion en el punto ----
  for(i in 1:(n-1)) {
    if(x[i]<= xk & x[i+1]>= xk){
      s = i}
  } #Nos fijamos a que polinomio pertenece el valor a interpolar
  yk = fx[s]+b[s]*(xk-x[s])+c[s]*(xk-x[s])^2+d[s]*(xk-x[s])^3 #Lo valuamos en el polinomio correspondiente
  return(yk)
}


Interpolo_en_pto(A, 12.2416)

Interpolo_en_pto(A, 0)

# 2. ----
 # a. 
Derivada_3P_PuntoExtremo<-function(n,x,f){
  h=x[2]-x[1]
  if(n==1){
    return((1/(2*h))*(-3*f[n]+4*f[n+1]-f[n+2]))
  } else if (n==2){
    return(paste("No es posible calcular f'(x_0) por este método."))
  } else {
    return((1/(2*h))*(3*f[n]-4*f[n-1]+f[n-2]))
  }
}

x = c(0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12)
f = c(115.3371, 109.7031, 104.8099, 100.0653, 95.3516, 91.1998, 87.2676, 83.5825, 79.9116, 76.8435, 73.9473, 70.0320, 67.7022)

# Derivada de 0.04
Derivada_3P_PuntoExtremo(5, x, f)

# Derivada de 0.03
Derivada_3P_PuntoExtremo(4, x, f)

# b.
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

SegundaDerivada(x, f)
print(paste("P''(0.04) =", 5619))
print(paste("P''(0.03) =", 309))

fx_prima = Derivada_3P_PuntoExtremo(5, x, f)

Derivada_segunda = function(x, fx, n, h){
  m = length(fx)
  fx_prima
  
  if(missing(n)){
    for (i in 2:(n - 1)) {
      fx_prima[i] = (fx[i-1] - 2*fx[i] + fx[i+1])/(h^2)
    }
    
    tabla = cbind.data.frame(x, y, fx_prima)
    return(tabla)
  }
  else{
    if(n == 1 || n == m){
      return("Este valor no se puede calcular")
    }
    else{
      fx_prima[n] = (fx[n-1] - 2*fx[n] + fx[n+1])/(h^2)
      return(fx_prima[n])
    }
  }
}

h = diff(x)[1]
x = c(0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12)
f = c(115.3371, 109.7031, 104.8099, 100.0653, 95.3516, 91.1998, 87.2676, 83.5825, 79.9116, 76.8435, 73.9473, 70.0320, 67.7022)

Derivada_segunda(x, f, 5, h)

fx_prima = Derivada_3P_PuntoExtremo(4, x, f)
Derivada_segunda = function(x, fx, n, h){
  m = length(fx)
  fx_prima
  
  if(missing(n)){
    for (i in 2:(n - 1)) {
      fx_prima[i] = (fx[i-1] - 2*fx[i] + fx[i+1])/(h^2)
    }
    
    tabla = cbind.data.frame(x, y, fx_prima)
    return(tabla)
  }
  else{
    if(n == 1 || n == m){
      return("Este valor no se puede calcular")
    }
    else{
      fx_prima[n] = (fx[n-1] - 2*fx[n] + fx[n+1])/(h^2)
      return(fx_prima[n])
    }
  }
}

h = diff(x)[1]
x = c(0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12)
f = c(115.3371, 109.7031, 104.8099, 100.0653, 95.3516, 91.1998, 87.2676, 83.5825, 79.9116, 76.8435, 73.9473, 70.0320, 67.7022)

Derivada_segunda(x, f, 4, h)

# 1 ----
# 1.1
Integral_ReglaTrapecio<-function(x,f){
  h=x[2]-x[1]
  return((h/2)*(f(x[1])+f(x[2])))
}


Integral_ReglaSimpson<-function(x,f){
  h=(x[2]-x[1])/2
  return((h/3)*(f(x[1])+4*f(x[1]+h)+f(x[1]+2*h)))
}

Integral_ReglaSimpson_TresOctavos<-function(x,f){
  h=(max(x)-min(x))/3
  return(((3*h)/8)*(f(x[1])+3*f(x[1]+h)+3*f(x[1]+2*h)+f(x[1]+3*h)))
}

x = c(1.15, 4.99)
f = function(x){(4.36^2*(x/2.52)^4.36)/(x*(1+(x/2.52)^4.36)^(4.36 +1))}

Integral_ReglaTrapecio(x, f)
# Nodos y0, y1
Integral_ReglaSimpson(x, f)
# Nodos y0, y1, y2
Integral_ReglaSimpson_TresOctavos(x, f)
# Nodos y0, y1, y2, y3

# 1.2 
ReglaCompuestaTrapezoidal<-function(a,b,n,f){
  h=(b-a)/n
  I1=0
  for(i in 1:(n-1)){
    I1=I1+f(a+i*h)
  }
  return(h/2*(f(a)+2*I1+f(b)))
}

ReglaCompuestaTrapezoidal(1.15, 4.99, 17, f)
# 0.8646727 para nodos y0, y1

funcion = expression((4.36^2*(x/2.52)^4.36)/(x*(1+(x/2.52)^4.36)^(4.36 +1)))

x  =  seq(1.15, 4.99, length.out = 10000)
fx = eval(D(D(funcion, "x"),"x"), list(x = x))

limitesuperior = 4.99
limiteinferior = 1.15
n = 17
h = (limitesuperior - limiteinferior)/n

# Cota de error: 0.0606897
((limitesuperior - limiteinferior)/12)*(h^2)* max(abs(fx))

# El metodo compuesto es mas exacto, ya que en este caso usa 17 subintervalos
# dentro del intervalo seleccionado, generando un trapecio por intervalo
# con una menor cota de error

# 1.3 
EFuncion = function(x){x*((4.36^2*(x/2.52)^4.36)/(x*(1+(x/2.52)^4.36)^(4.36 +1)))}

ReglaCompuestaTrapezoidal(1.15, 4.99, 474, EFuncion)

funcionE = expression((x*(4.36^2*(x/2.52)^4.36)/(x*(1+(x/2.52)^4.36)^(4.36 +1))))

x  =  seq(1.15, 4.99, length.out = 10000)
fx = eval(D(D(funcionE, "x"),"x"), list(x = x))

limitesuperior = 4.99
limiteinferior = 1.15
n = 474
h = (limitesuperior - limiteinferior)/n

# Cota de error: 0.0001449476
((limitesuperior - limiteinferior)/12)*(h^2)* max(abs(fx))

# 1.4 #ME DA VARIANZA NEGATIVA, MAL
Esperanza = ReglaCompuestaTrapezoidal(1.15, 4.99, 474, EFuncion)
Varianza = function(x){(x^2*((4.36^2*(x/2.52)^4.36)/(x*(1+(x/2.52)^4.36)^(4.36 +1)))) - Esperanza^2}
ReglaCompuestaTrapezoidal(1.15, 4.99, 474, Varianza)
# 4. ----
set.seed(895700)
P0 = 73
mu = 0.13
sigma = 0.1
T = 0.5
n = 182.5
dt = T/n

#Simulación de Caminos de Precios
m = 1233
Pt = matrix(NA, nrow = m, ncol = n+1) 
Pt[,1] = P0
for (i in 1:m) {
  for (t in 2:(n+1)){
    Pt[i,t] = Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
  } 
} 

k=length(Pt[1,])
P = mean(Pt[,k])
Desvio = sd(Pt[,k])
P #105.191
Desvio #11.19822
hist(Pt)

#Precio final
set.seed(895700)
e = rnorm(m) 
PT = matrix(NA,nrow = m, ncol = 1) 
PT = P0 * exp((mu - 0.5*sigma^2)*T + sigma * sqrt(T) * e) 
mean(PT)
sd(PT)
hist(PT, main = "Simulacion del precio final",xlab = "Precios simulados", ylab = "Frecuencia")


# 4.2 Probabilidad: Precio final entre 38 y P0
df_Pt = as.tibble(Pt[,ncol(Pt)])

a = df_Pt %>% filter(value > 38) %>%  filter(value < P0)

nrow(a) / nrow(df_Pt)

# 4.3: Precio final PT sea mayor al esperado
Te = mean(PT)
a = df_Pt %>% filter(value > Te) 

nrow(a) / nrow(df_Pt)

# 5 ----
# a. 
## Metodo de Euler 
fn = function(t,y){     
  return(cos(y)/t^1.69 + t/y^3)
}

Metodo_Euler = function(a, b, N, alfa){ # alfa = y0
  h = (b - a)/N
  w = c(rep(NA, N + 1))
  w[1] = alfa
  t = c(seq(a, b, h))
  t[1] = a
  for (i in 1:N) {
    w[i + 1] = w[i] + h * fn(t[i], w[i])
  }
  return(data.frame(t,w))
}

Metodo_Euler(4, 5, 45, 0.46)
Metodo_E = Metodo_Euler(4, 5, 45, 0.46)

# b. 
runge_kutta = function(a, b, N, alfa){
  h = (b - a)/N
  w = c(rep(NA, N + 1))
  w[1] = alfa
  t = c(seq(a, b, h))
  t[1] = a
  for (i in 1:N) {
    k1 = h*fn(t[i], w[i])
    k2 = h*fn(t[i] + h/2, w[i] + k1/2)
    k3 = h*fn(t[i] + h/2, w[i] + k2/2)
    k4 = h*fn(t[i + 1], w[i] + k3)
    w[i + 1] = w[i] + (k1 + 2*k2 + 2*k3 + k4)/6
    
  }
  return(data.frame(t,w))
}
runge_kutta(4, 5, 45, 0.46)

Metodo_RK = runge_kutta(4, 5, 45, 0.46)

DF_Graf = data.frame(Metodo_RK$t, Metodo_RK$w, Metodo_E$w)
colnames(DF_Graf) <- c("t","Runge_Kutta","Euler")
colors <- c("Runge_Kutta" = "#69b3a2", "Euler" = "pink")

#Grafico 

Grafico_RungeKutta_Euler <-ggplot(data = DF_Graf,
                                  aes(x = t,))+
  geom_point(aes(y = Runge_Kutta, color = "Runge_Kutta"),size=1)+ 
  geom_point(aes(y = Euler, color = "Euler"), size = 1)+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  ggtitle("Metodo Runge Kutta & Euler")+ 
  xlab("t")+ 
  ylab("y")+
  labs(colour = "Legend")+
  scale_color_manual(values = colors)+
  theme_classic()

Grafico_RungeKutta_Euler

# d 
# Los métodos de Taylor de orden superior (como Runge-Kutta) tienen error de truncamiento de orden alto, lo que hace que aproximen mejor,
# pero es necesario conocer las derivadas. En cambio, el método de Euler, lo hace mas "económico" porque no es necesario conocer las derivadas
# pero esto lo hace menos exacto.