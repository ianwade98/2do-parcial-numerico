#Cuatrimestre 1, 2021 - Segundo Parcial

# Ingrese en este bloque de c�digo las librer�as que utilizar�
library(ggplot2)
library(scales)
library(flextable)
library(tidyverse)

```

\newpage

# Integral Log-Normal (25 puntos)

```{r Ejercicio1-Datos}
# No edite este bloque de c�digo

Considere la siguiente funci�n de densidad:
  
  $$f_S(x) = \frac{1}{x \sigma \sqrt{2 \pi}}e^{-\frac{ (\ln{(x)} -\mu)^2 }{2 \sigma^2}};\text{con } x>0.$$
  
  Para su ejercicio particular, considere $\mu = `r mu1`$ y $\sigma = `r sigma1` \sqrt{0.75}$.

## Simpson y Trapecio

Aproxime la probabilidad de que S est� entre $`r a1`$ y $`r b1`$ usando los m�todos de "Trapecio", "Simpson" y "Simpson tres octavos". Indique en cada caso los "nodos" $x_0, x_1, ., x_n$ que se utilizan para la aproximaci�n.

Respuesta:
  
  ```{r Respuesta-Ej1-1, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 1.1

newtonCotesCerradas <- function(limiteInferior, limiteSuperior, funcion, n){
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
  else if (n == 2){
    return((h/3) * (fx[1] + 4*fx[2] + fx[3]))
  }
  else if(n == 3){
    return((3/8)*h*(fx[1] + 3*fx[2] + 3*fx[3] + fx[4]))
  }
  
}

# n = 1. Regla del trapecio.
# n = 2. Regla de Simpson.
# n = 3. Regla de tres octavos de Simpson.
# Poner la funcion con "x" como incognita
mu <- 2.77
sigma <- 0.22*sqrt(0.75)

newtonCotesCerradas(limiteInferior = 12.68, limiteSuperior = 20.42, funcion = expression( (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), n = 1)  

print("Nodos en trapecio: x0, x1")

newtonCotesCerradas(limiteInferior = 12.68, limiteSuperior = 20.42, funcion = expression( (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), n = 2)  

print("Nodos en Simpson: x0, x1, x2")

newtonCotesCerradas(limiteInferior = 12.68, limiteSuperior = 20.42, funcion = expression( (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), n = 3)  

print("Nodos en Simpson 3/8: x0, x1, x2, x3")

x <- seq(from = 0.1, to = 50, by = 0.1)
fx <- eval(expression( (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), list(x = x))

ggplot() +
  geom_line(aes(x = x, y = fx))
```

## Simpson Compuesto

Aproxime el punto anterior usando "Simpson Compuesto", con $n= `r size1`$. Calcule la cota del error.

Respuesta:
  
  ```{r Respuesta-Ej1-2, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 1.2

IntegracionCompuesta <- function(limiteInferior, limiteSuperior, funcion, n, cantIntervalos){
  
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
    
    if(n == 2){
      resultado <- resultado + (h/3) * (fx[1] + 4*fx[2] + fx[3])
    }
    
    limiteInferior <- limiteSuperior
  }
  
  return(resultado)
}

sigma <- 0.22*sqrt(0.75)

# n = 2. Simpson
IntegracionCompuesta(limiteInferior = 12.68, limiteSuperior = 20.42, funcion = expression( (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), cantIntervalos = 1000, n = 2)

x <- seq(from = 12.68, to = 20.42, by = 0.001)

fx <- eval(D(D(D(D(expression( (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), "x"),"x"),"x"),"x"), list(x = x))

ggplot() +
  geom_line(aes(x = x, y = fx))

limiteSuperior <- 20.42
limiteInferior <- 12.68
n <- 1000

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/180) * (h^4) * max(abs(fx))

```

## Aproximaci�n Num�rcica de Esperanza Matem�tica

Use "Trapecio Compuesto" con $n = `r size1`$ para aproximar $E(S)$; es decir, la esperanza matem�tica de S. Calcule la cota del error.

Respuesta:
  
  ```{r Respuesta-Ej1-3, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 1.3
IntegracionCompuesta <- function(limiteInferior, limiteSuperior, funcion, n, cantIntervalos){
  
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

# n = 1. Trapecio
# n = 2. Simpson
IntegracionCompuesta(limiteInferior = 12.68, limiteSuperior = 20.42, funcion = expression(x * (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), cantIntervalos = 1000, n = 1)

x <- seq(from = 12.68, to = 20.42, by = 0.001)

fx <- eval(D(D(expression( (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), "x"),"x"), list(x = x))

ggplot() +
  geom_line(aes(x = x, y = fx))

limiteSuperior <- 20.42
limiteInferior <- 12.68
n <- 1000

h <- (limiteSuperior - limiteInferior)/n

((limiteSuperior - limiteInferior)/12) * (h^2) * max(abs(fx)) 

```

## Aproximaci�n Num�rcica de Esperanza Matem�tica de una funci�n condicional

Use "Simpson Compuesto" con $n =$ `r size1` para aproximar $E[max(S-40,0)]$; es decir, la esperanza matem�tica de una funci�n condicional que toma S-40 siempre y cuando sea positivo, y cero en caso contrario.

Respuesta:
  
  ```{r Respuesta-Ej1-4, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 1.4

IntegracionCompuesta <- function(limiteInferior, limiteSuperior, funcion, n, cantIntervalos){
  
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
    
    if(n == 2){
      resultado <- resultado + (h/3) * (fx[1] + 4*fx[2] + fx[3])
    }
    
    limiteInferior <- limiteSuperior
  }
  
  return(resultado)
}

sigma <- 0.22*sqrt(0.75)
mu <- 2.77

# n = 2. Simpson
# esperanza <- if( a <- IntegracionCompuesta(limiteInferior = 12.68, limiteSuperior = 20.42, funcion = expression(x * (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), cantIntervalos = 1000, n = 2) - 40 > 0){a}else{0}
# 
# esperanza

# Esto lo hizo dario en clase

f <- function(x){
  if(x>40){
    return(x-40)
  } else {
    return(0)
  }
}

x1 <- seq(0,100)
y <- sapply(x1, f)
plot(x1,y)

u <- function(x){ ifelse(x>40, x-40, 0)}

IntegracionCompuesta(limiteInferior = 10^-10, limiteSuperior = 100, funcion = expression(u(x) * (1/(x*sigma*sqrt(2*pi))) * exp(- ((log(x)-mu)^2)/(2*sigma^2)) ), cantIntervalos = 1000, n = 2)

```

\newpage

# Simulaci�n de Precios (25 puntos)

```{r Ejercicio2-Datos}
# No edite este bloque de c�digo
set.seed(NroReg)
Precio2 = sample(x = c(100,250,300), size = 1)
mu2 = round(rnorm(1,0.15,0.05),2)
sigma2 = round(rnorm(1,0.20,0.02),2)
T2 = 0.75
a2 = round(Precio2 * exp(mu2*T2 + sigma2*sqrt(T2)*qnorm(runif(1,0,0.25))),2)
b2 = round(Precio2 * exp(mu2*T2 + sigma2*sqrt(T2)*qnorm(runif(1,0.75,1))),2)
size2a = sample(x = c(10000,15000,20000), size = 1)
size2b = sample(x = c(1000,1500,2000), size = 1)
```

Considere el siguiente modelo para simular precios:
  
  $$
  P_{t+\Delta t} = P_t \times \exp{[(\mu - 0.5 \sigma^2) \Delta t + \sigma \sqrt{\Delta t} \epsilon ]}
$$ 
  
  $$
  P_{T} = P_0 \times \exp{[(\mu - 0.5 \sigma^2) T + \sigma \sqrt{T} \epsilon ]}
$$ 
  
  donde $\epsilon$ es una variable aleatoria normal est�ndar. Para su ejercicio particular, considere $P_0 = `r Precio2`$, $T=0.75$, $\mu = `r mu2`$ y $\sigma = `r sigma2`$.

## Simulaci�n de $P_T$

Realice $`r comma(size2a)`$ simulaciones de precios en el momento $T$, $P_T$. Almacene dicha simulaci�n en una matriz (o *data frame*) llamada `PT`. Grafique un histograma de los precios simulados y calcule el Precio Esperado y el Desv�o Est�ndar.

***Nota***: Debe simular solamente precios finales (no debe usar "caminos de precios").

Respuesta: Precio esperado :  y Desvio Estandar : 
  
  ```{r Respuesta-Ej2-1, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 2.1

p0 <- 300
mu <- 0.1
sigma <- 0.18

# 0.75 a�o
anios <- 0.75

# Simulacion
m <- 15000

# Matriz de camino de precios
pt <- matrix(NA, nrow = m, ncol = 1)

#set.seed(895096)
e <- rnorm(m)

pt[,1] <- p0 * exp((mu-0.5*sigma^2) * anios + sigma*sqrt(anios)*e)

mean(pt)
sd(pt)

ggplot() +
  geom_histogram(aes(pt[,1])) +
  geom_vline(xintercept = mean(pt), colour = "darkblue") +
  xlab("Price") + ylab("Times")


```

## Simulaci�n de caminos de precios $P_t$ con $t>0$

Realice $`r size2b`$ simulaciones de ***caminos de precios*** desde $t=0$ hasta $t=T$, con $\Delta t = 1/250$. Almacene los resultados en una matriz (o *data frame*) llamada `P0aT`. Grafique un histograma de los **precios finales** simulados $P_T$ y calcule el $E(P_T)$ y $d.e.(P_T)$ (desv�o est�ndar).

Respuesta: Valor esperdo = 270.7219 y Desvio Estandar = 44.10201

```{r Respuesta-Ej2-2, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 2.2

p0 <- 300
mu <- 0.1
sigma <- 0.18

# 0.75 a�o
anios <- 0.75

dt <- 1/250

# Simulacion
m <- 2000

# Time steps
n <- 187.5

# Matriz de camino de precios
P0aT <- matrix(NA, nrow = m, ncol = n+1)

P0aT[,1] <- p0

for (i in 1:m) {
  for(t in 2:(n+1)){
    P0aT[i,t] <- P0aT[i, t-1] * exp((mu-0.5*sigma^2) * dt + sigma * sqrt(dt) * rnorm(1))
  }
}

mean(P0aT[,ncol(P0aT)])
sd(P0aT[,ncol(P0aT)])

ggplot() +
  geom_histogram(aes(P0aT[,ncol(P0aT)]), binwidth = 5) +
  geom_vline(xintercept = mean(P0aT[,ncol(P0aT)]), colour = "darkblue") +
  xlab("Price") + ylab("Times")
```

## Gr�fico de caminos de precios $P_t$ con $t>0$ (incluyendo $E[P_t]$ e IC)

Con los resultados del punto anterior, grafique todos los caminos simulados con colores al azar. Incluya con l�nea gruesa de color **negro** el camino del valor esperado y con l�neas gruesas de color **azul** el camino de un Intervalo de Confianza con 99% de probabilidad.

Respuesta:
  
  ```{r Respuesta-Ej2-3, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 2.3
# Gr�fico 

t <- rep(0:n, m)
t <- matrix(t, nrow = m, ncol = n+1, byrow = T)

plot(t[1,], P0aT[1,], type = "l", ylim = c(min(P0aT), max(P0aT)))
for (i in 2:m) {
  lines(t[i,], P0aT[i,], col = trunc(runif(1)*m))
}

# Medias
m <- matrix(NA, nrow = 1, ncol = n+1)
for (i in 1:(n+1)) {
  m[i] <- mean(P0aT[,i])
}

# IC
prob <- 0.99
ls <- matrix(NA, nrow = 1, ncol = n+1)
for (i in 1:(n+1)) {
  ls[i] <- quantile(P0aT[,i], prob)
}

li <- matrix(NA, nrow = 1, ncol = n+1)
for (i in 1:(n+1)) {
  li[i] <- quantile(P0aT[,i], 1-prob)
}

lines(t[1,], m, col = 'black', lwd = 5)
lines(t[1,], ls, col = 'blue', lwd = 5)
lines(t[1,], li, col = 'blue', lwd = 5)
```

## Estimaci�n de probabilidades a partir de muestras simuladas

Usando las matrices `PT` (del punto 2.1) y `P0aT` (del punto 2.2), realice dos estimaciones de la probabilidad de que el precio final $P_T$ (con $T=0.75$) sea menor a $`r a2`$ y dos estimaciones de la probabilidad de que el precio sea mayor a $`r b2`$. Compare los resultados obtenidos usando `PT` (del punto 2.1) con los resultados usando `P0aT` (del punto 2.2)

Respuesta:
  
  ```{r Respuesta-Ej2-4, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 2.4

# Hacemos el supuesto de que los precios no pueden ser menores a cero

# Tengo dos ideas:
# 1) A partir de las matrices genero un polinomio mediante interpolaci�n (Ej: Spline) y con eso calculo una intergal
# 2) Calculo la proporci�n sobre el total


df_pt <- as.data.frame(pt)

punto1 <- rep(NA, times = 2)

a <- df_pt %>% filter(V1 < 268.95)

punto1[1] <- nrow(a) / nrow(df_pt)

df_p0aT <- as.tibble(P0aT[,ncol(P0aT)])

a <- df_p0aT %>% filter(value < 268.95)

punto1[2] <- nrow(a) / nrow(df_p0aT)

# Mayor a 364.79

df_pt <- as.data.frame(pt)

punto2 <- rep(NA, times = 2)

a <- df_pt %>% filter(V1 > 364.79) 

punto2[1] <- nrow(a) / nrow(df_pt)

df_p0aT <- as.tibble(P0aT[,ncol(P0aT)])

a <- df_p0aT %>% filter(value > 364.79)

punto2[2] <- nrow(a) / nrow(df_p0aT)


df_resultado <- data.frame("menor a 268.95" = punto1, "mayor a 364.79" = punto2)

rownames(df_resultado) <- c("pt", "p0aT")


df_resultado
```

\newpage

# Integraci�n y Derivaci�n Num�rica (20 puntos)

```{r Ejercicio3-Datos}
# No edite este bloque de c�digo
set.seed(NroReg)
alpha = sample(x = c(1,2.5,3), size = 1)
beta = sample(x = c(1,2.5,3), size = 1)
```

Considere la siguiente funci�n de densidad de la variable aleatoria $Y$, con dominio en el intervalo $Y \in (0;1)$, y par�metros $\alpha = `r alpha`$ y $\beta = `r beta`$:
  
  $$
  f_Y(x|\alpha,\beta) = \frac{\Gamma(\alpha+\beta)}{\Gamma(\alpha)\Gamma(\beta)}x^{\alpha-1}(1-x)^{\beta-1}
$$ 
  
  La funci�n $\Gamma(z)$ est� definida por la siguiente integral: $\Gamma(z) = \int_{0}^{\infty}t^{z-1}e^{-t}dt$.

## $E(Y|\alpha;\beta)$

Calcule mediante una integral num�rica, usando Simpson Compuesto con $n = 100$, la esperanza matem�tica de la variable aleatoria $Y$. 
Los valores de la funci�n $\Gamma(z)$ que se incluyen en la funci�n de densidad tambi�n debe estimarlos num�ricamente usando Simpson Compuesto con $n = 1000$ y un valor de l�mite superior que considere adecuado.

```{r Respuesta-Ej3-1, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 3.1

IntegracionCompuesta <- function(limiteInferior, limiteSuperior, funcion, n, cantIntervalos){
  
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
    
    if(n == 2){
      resultado <- resultado + (h/3) * (fx[1] + 4*fx[2] + fx[3])
    }
    
    limiteInferior <- limiteSuperior
  }
  
  return(resultado)
}


alfa <- 3
beta <- 1

gamma_alfa_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa+beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_alfa <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1, funcion = expression( x * (gamma_alfa_beta/(gamma_alfa * gamma_beta)) * x^(alfa-1) * (1-x)^(beta-1) ), n = 2, cantIntervalos = 100)

```

## Derivada respecto a $\alpha$

Estime num�ricamente la derivada de $E(Y|\alpha;\beta)$ respecto del par�metro $\alpha$.

Vuelvo a la definici�n cl�sica de derivada:
  \begin{equation*}
\displaystyle \lim_{x \to \infty} f'(x) = \dfrac{f(x+h)-f(x)}{h}
\end{equation*}

```{r Respuesta-Ej3-2, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 3.2

alfa <- 3
beta <- 1

gamma_alfa_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa+beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_alfa <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

fx <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1, funcion = expression( x * (gamma_alfa_beta/(gamma_alfa * gamma_beta)) * x^(alfa-1) * (1-x)^(beta-1) ), n = 2, cantIntervalos = 100)

alfa <- 3 + 10^-10

gamma_alfa_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa+beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_alfa <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

fx_h <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1, funcion = expression( x * (gamma_alfa_beta/(gamma_alfa * gamma_beta)) * x^(alfa-1) * (1-x)^(beta-1) ), n = 2, cantIntervalos = 100)

(fx_h - fx) / (10^-10)

```

## Derivada respecto a $\beta$

Estime num�ricamente la derivada de $E(Y|\alpha;\beta)$ respecto del par�metro $\beta$.

```{r Respuesta-Ej3-3, echo=TRUE}
# Ingrese a continuaci�n, en este bloque, todo el c�digo necesario para resolver el ejercicio 3.3

alfa <- 3
beta <- 1

gamma_alfa_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa+beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_alfa <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

fx <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1, funcion = expression( x * (gamma_alfa_beta/(gamma_alfa * gamma_beta)) * x^(alfa-1) * (1-x)^(beta-1) ), n = 2, cantIntervalos = 100)

beta <- 1 + 10^-10

gamma_alfa_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa+beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_alfa <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(alfa-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

gamma_beta <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1000, funcion = expression( x^(beta-1) * exp(-x) ), n = 2, cantIntervalos = 1000)

fx_h <- IntegracionCompuesta(limiteInferior = 0, limiteSuperior = 1, funcion = expression( x * (gamma_alfa_beta/(gamma_alfa * gamma_beta)) * x^(alfa-1) * (1-x)^(beta-1) ), n = 2, cantIntervalos = 100)

(fx_h - fx) / (10^-10)

print("gamma_beta no puede ser < 1 porque si no la funci�n no converge a ning�n n�mero.")
```

\newpage

# SQL (20 Puntos)

Utilice la base de datos de la siguiente web para preparar sus c�digos de SQL: <https://www.w3schools.com/sql/trysql.asp?filename=trysql_select_all>

## Consulta Clientes

Escriba una consulta SQL que contenga los clientes de Estados Unidos y Alemania que gastaron m�s de \$10.000 en el total de todas sus compras. La salida debe contener los siguientes campos: Nombre del Cliente, ciudad y pa�s del cliente, Cantidad de compras, Total (calculado como la suma total del Precio\*Cantidad).

Respuesta (SELECT c.CustomerName as NombreCliente, c.City as Ciudad, c.Country as Pais, count(DISTINCT o.OrderID) as CantidadCompra,
ROUND(sum(p.Price*d.Quantity),2) as Total
FROM Customers c
INNER JOIN Orders o ON c.CustomerID=o.CustomerID
INNER JOIN OrderDetails d ON o.OrderID=d.OrderID
INNER JOIN Products p ON d.ProductID=p.ProductID
WHERE Country = 'USA'
GROUP BY c.CustomerName
HAVING sum(p.Price*d.Quantity) > 10000
UNION
SELECT c.CustomerName as NombreCliente, c.City as Ciudad, c.Country as Pais, count(DISTINCT o.OrderID) as CantidadCompra,
ROUND(sum(p.Price*d.Quantity),2) as Total
FROM Customers c
INNER JOIN Orders o ON c.CustomerID=o.CustomerID
INNER JOIN OrderDetails d ON o.OrderID=d.OrderID
INNER JOIN Products p ON d.ProductID=p.ProductID
WHERE Country = 'Alemania'
GROUP BY c.CustomerName
HAVING sum(p.Price*d.Quantity) > 10000
):

## Consulta Ventas

Escriba una consulta SQL que contenga todas las ventas realizadas por Robert King a los clientes de Espa�a, M�xico y Canad�. La salida debe contener los siguientes campos: Nombre y Apellido del Vendedor, Nombre del Cliente, Ciudad y Pa�s del Cliente, ID de la Orden de Compra (tabla Orders), Nombre del Producto (tabla Products), Presentaci�n (Unit, de tabla Products), Cantidad (tabla OrderDetails), Precio (tabla Products) y Total (calculado como Precio\*Cantidad).

Respuesta (SELECT E.LastName as Apellido, FirstName as Nombre, U.CustomerName as NombreCliente,U.City as Ciudad, U.Country as Pais, R.OrderID as IDOrder, P.ProductName as NombreProducto, P.Unit as Presentacion, O.Quantity as Cantidad, P.price as Precio, round( P.Price*(O.Quantity),2) as Total
FROM Products P
INNER JOIN OrderDetails O ON P.ProductID = O.ProductID
INNER JOIN Orders R ON O.OrderID = R.OrderID
INNER JOIN Employees E ON R.EmployeeID = E.EmployeeID
INNER JOIN Customers U ON R.CustomerID = U.CustomerID
WHERE LastName = 'King' AND Country = 'Spain'
UNION
SELECT E.LastName as Apellido, FirstName as Nombre, U.CustomerName as NombreCliente,U.City as Ciudad, U.Country as Pais, R.OrderID as IDOrder, P.ProductName as NombreProducto, P.Unit as Presentacion, O.Quantity as Cantidad, P.price as Precio, round( P.Price*(O.Quantity),2) as Total
FROM Products P
INNER JOIN OrderDetails O ON P.ProductID = O.ProductID
INNER JOIN Orders R ON O.OrderID = R.OrderID
INNER JOIN Employees E ON R.EmployeeID = E.EmployeeID
INNER JOIN Customers U ON R.CustomerID = U.CustomerID
WHERE LastName = 'King' AND Country = 'Canada'
UNION
SELECT E.LastName as Apellido, FirstName as Nombre, U.CustomerName as NombreCliente,U.City as Ciudad, U.Country as Pais, R.OrderID as IDOrder, P.ProductName as NombreProducto, P.Unit as Presentacion, O.Quantity as Cantidad, P.price as Precio, round( P.Price*(O.Quantity),2) as Total
FROM Products P
INNER JOIN OrderDetails O ON P.ProductID = O.ProductID
INNER JOIN Orders R ON O.OrderID = R.OrderID
INNER JOIN Employees E ON R.EmployeeID = E.EmployeeID
INNER JOIN Customers U ON R.CustomerID = U.CustomerID
WHERE LastName = 'King' AND Country = 'Mexico'
):

\newpage

# Aprendizaje Autom�tico (10 puntos)

Defina el aprendizaje autom�tico y mencione qu� ventajas se pueden obtener de su utilizaci�n. Explique brevemente los dos tipos de m�todos m�s utilizados en la actualidad.

Respuesta (ML es un conjunto de t�cnicas de programaci�n y m�todos, que permiten el an�lisis de grandes vol�menes de datos y detectar autom�ticamente patrones sin intervenci�n humana, basados en experiencia pasada.  Debido al Big Data, las organizaciones precisan del ML para aprovechar los datos que se obtienen de las fuentes externas, como por ej redes sociales, datos del supermercado, etc, para poder mejorar, optimizar y automatizar procesos para una mejor toma de decisiones. Algunas de las utilidades cotidianas se aplican justamente en las compras en el supermercado en lo que llamamos segmentacion de clientes, y por ejemplo en el correo electronico cuando distinguimos entre spam y no spam. Esto lo podemos lograr gracias a los metodos que se utilizan hoy en dia  :el aprendizaje supervisado y el aprendizaje no supervisado. En el supervisado se le asigna a cada salida una etiqueta (tenemos 1000 mails, separados en dos grupos a los cuales les asigno la etiqueta de spam o no spam). En el no supervisado los datos no tienen etiquetas asociadas (tenemos los dos grupos de mails , pero no poseen la etiqueta de cual es spam y cual no) )


