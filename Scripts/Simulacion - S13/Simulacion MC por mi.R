## MONTE CARLO 

# Integral en un Intervalo entre 0 y 1 ----
f1 <- function(x){x^2} #Funcion a integrar
n <- 100 # Tamanio muestra
U <- runif(n) #Generacion de numeros uniformes
Integral.MC <- 1/n*sum(f1(U)) # Estimacion de la integral
sf.MC <- sqrt(1/(n-1))*sum((f1(U)-Integral.MC)^2) #Desvio estimado
Error.MC <- sf.MC/sqrt(n) #Error de estimacion: Esperanza estimada - Esperanza real 

Integral.MC #0.3356721
Error.MC #0.1025405

# Integral en un intervalo entre a (inferior) y b (superior) ----
f1 <- function(x){x^2}
n <- 10000
a <- 10
b <- 15
U <- a+(b-a)*runif(n)

# Valor esperado E[f(U)] da la altura media.
Altura.Promedio <- 1/n * sum(f1(U))
Ancho.Base <- b-a

Integral.MC.ab <- Altura.Promedio*Ancho.Base

#Calculo el error estimado
sf.MC.ab <- sqrt(1/(n-1)*sum((f1(U)*(b-a)-Integral.MC.ab)^2))
Error.MC.ab <- sf.MC.ab/sqrt(n)

# Integral de una normal: mismo codigo, distinta f(x) ----
mu = 5
sigma = 10
f2 <- function(x){1/ sqrt((2*pi*sigma^2)) * exp(-(x-mu)^2/(2*sigma^2))}
n = 100
a = 10
b = 20
U <- a+(b-a)*runif(n)
Altura.Promedio <- 1/n * sum(f2(U))
Ancho.Base <- b-a

Integral.MC.ab <- Altura.Promedio*Ancho.Base
sf.MC.abN <- sqrt(1/(n-1)*sum((f2(U)*(b-a)-Integral.MC.ab)^2))
Error.MC.abN <- sf.MC.abN/sqrt(n)

Integral.MC.ab
sf.MC.abN
Error.MC.abN

# Para la integral, la solucion analitica con R es 
pnorm(20, 5, 10)-pnorm(10, 5, 10)

#Nota
INT.SUP = Integral.MC.ab+2*Error.MC.abN
INT.INF = Integral.MC.ab-2*Error.MC.abN


# Simulacion de numero de siniestros con distribucion compuesta ----

#Genero un número aleatorio N con distribución de Poisson de lambda=50
N<-rpois(n=1,lambda=50)
#Genero un vector con N números aleatorios con distribución Gamma 
Xi=rgamma(n=N,shape=10,scale=5)
S=sum(Xi)

#Ingreso el tamaño de la muestra
M=1000

#Creo la matriz de salida
Resultado=matrix(NA,nrow=M,ncol=2)

for(m in 1:M){
  N=rpois(1,lambda=50)
  Resultado[m,1]<-N
  Xi=rgamma(n=N,shape=10,scale=5)
  S=sum(Xi)
  Resultado[m,2]=S
}

# Variables de resumen
N.E = mean(Resultado[,1]) #Media primera columna
N.var = var(Resultado[,1])#Varianza de la primera columna 
#En ambos casos de poisson, en ambos casos nos tiene que dar lamda

E.S = mean(Resultado[,2]) #Esperanza S
sd.S = sd(Resultado[,2])  #Desvio estandar S

hist(Resultado[,2])

# Grafico ---- 
x <- seq(from = 2, to = 6, by = 0.01)
fx <- f1(x)
x1 <- seq(from = 0, to = 8, by = 0.01)
fx1 <- f1(x1)
ggplot() +
  geom_area(aes(x = x, y = fx )) +
  geom_line(aes(x = x1, y = fx1))
