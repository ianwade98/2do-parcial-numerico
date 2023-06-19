rm(list = ls())
graphics.off()

# MODELO GEOMETRICO BROWNEANO

## SIMULACION DEL PRECIO DE UN ACTIVO FINANCIERO 

### CAMINO DE PRECIOS ----

# Supuesto de MGB - Parametros 

P0 = 45
mu = 0.10
sigma = 0.20
T = 0.5
n = 182 
dt = T/n

#Simulación de Caminos de Precios
m = 1000  
Pt = matrix(NA, nrow = m, ncol = n+1) 

Pt[,1] = P0

for (i in 1:m) {
  for (t in 2:(n+1)){
    Pt[i,t] = Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
  } 
} 

#### Gráficos ----

t = rep(0:n, m)  
t = matrix(t,nrow = m,ncol = n+1,byrow = T) 
plot(t[1,], Pt[1,], type="l", ylim = c(min(Pt),max(Pt))) 
for (i in 2:m) {
  lines(t[i,], Pt[i,], col = trunc(runif(1)*m))
}

#### Métricas Estadísticas ----

# Vector de Promedios de cada uno de los momentos
M = matrix(NA,nrow=1,ncol = n+1)
for (i in 1:(n+1)) {
  M[i]<-mean(Pt[,i]) 
}

# Si quiero directo el promedio: 
k=length(Pt[1,])
P = mean(Pt[,k])
Desvio = sd(Pt[,k])
P #
Desvio #
hist(Pt)

#Vector de Percentiles
prob = 0.95   
#limite superior que acumula ese valor
LS = matrix(NA, nrow = 1, ncol = n+1)
for (i in 1:(n+1)) {
  LS[i] = quantile(Pt[,i],prob) 
}

#limite inferior que acumula ese valor
LI = matrix(NA, nrow = 1, ncol = n+1)
for (i in 1:(n+1)) {
  LI[i] = quantile(Pt[,i],1-prob)
}


#Agrego al grafico, con lines, la media y perc.

lines(t[1,], LS, col = 'red',lwd = 5) 
lines(t[1,], LI, col = 'red',lwd = 5) 
lines(t[1,], M, col = 'black',lwd = 5)

LS[183] 
M[183]
LI[183]


### PRECIO FINAL SIN CAMINO DE PRECIOS ----

# Parametros
P0 = 45
mu = 0.10    
sigma = 0.20   
T = 0.5         
n = 182         
dt = T/n

# Simulación Precio Final

m = 1000
e = rnorm(m) 
PT = matrix(NA,nrow = m, ncol = 1) 
PT = P0 * exp((mu - 0.5*sigma^2)*T + sigma * sqrt(T) * e) 
hist(PT)
hist(PT, main = "Simulacion del precio final",xlab = "Precios simulados", ylab = "Frecuencia")

P = mean(PT)
Desvio = sd(PT)
P #105.7745
Desvio #11.16131

#### Métricas Estadísticas ----
# Promedios
P = mean(PT)
Desvio = sd(PT)

# Probabilidad: Precio final entre 38 y P0
df_Pt = as.tibble(Pt[,ncol(Pt)])

a = df_Pt %>% filter(value > 38) %>%  filter(value < P0)

nrow(a) / nrow(df_Pt)

# Percentiles
prob = 0.95

LSPT = quantile(PT,prob)
LSPT

LIPT = quantile(PT,1-prob)
LIPT





## SIMULACION DEL PRECIO DE MULTIPLES ACTIVOS ----



### INDEPENDIENTES ----

# Parametros
P0 = c(45, 50, 108) 
mu = c(0.10, 0.15, 0.08) 
sigma = c(0.20, 0.25, 0.30) 
T = 0.5 
m = 1000 

z = matrix(rnorm(3*m), nrow = m, ncol=3) 
cor(z)

# Simulacion
P = matrix(NA, m, 3)
for (i in 1:m) {    
  for (k in 1:3) {
    P[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*z[i,k]) 
  }
}

P0.m = matrix(rep(P0,m),m,3,byrow=T)
P0.m
RL = log(P/P0.m)
RL
cor(RL)

#### Metricas Estadisticas ----

#Primer activo

mean(P[,1]) # Promedios
prob = 0.95  # Percentiles
LSP = quantile(P[,1],prob)
LSP
LIP = quantile(P[,1],1-prob)
LIP

#Segundo activo

mean(P[,2]) # Promedios 
prob = 0.95  # Percentiles
LSP = quantile(P[,2],prob)
LSP
LIP = quantile(P[,2],1-prob)
LIP


#Tercer Activo

mean(P[,3]) # Promedios
prob = 0.95  # Percentiles
LSP = quantile(P[,3],prob)
LSP
LIP = quantile(P[,3],1-prob)
LIP

#### Gráficos ----

par(mfrow=c(3,1))
hist(P[,1])
hist(P[,2])
hist(P[,3])

par(mfrow=c(1,3))
plot(P[,1],P[,2])
plot(P[,2],P[,3])
plot(P[,3],P[,1])

### CORRELACIONADOS ---- 

# Parametros

P0 = c(45, 50, 108) 
mu = c(0.10, 0.15, 0.08) 
sigma = c(0.20, 0.25, 0.30) 
T = 0.5 
m = 1000 

z = matrix(rnorm(3*m),nrow=m,ncol=3)

# Matriz Rho
Rho = diag(3)  
Rho[1,2] = Rho[2,1] = 0.90  
Rho[1,3] = Rho[3,1] = 0.70 
Rho[2,3] = Rho[3,2] = 0.60 
Rho

# Calculo Cholesky
CH = chol(Rho) 
CH
t(CH)%*%CH # Verifico

e = z%*%CH # Matriz e 
cor(e)

# A partir de los e, simulo
Pc = matrix(NA,m,3)
for (i in 1:m) { 
  for (k in 1:3) {  
    Pc[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*e[i,k])
  }
}

P0.m = matrix(rep(P0,m),m,3,byrow=T) 
RLc = log(Pc/P0.m)
cor(RLc) # Tiene que ser igual a cor(e), verifico
cor(e)
cor(RLc)-cor(e)

#### Metricas Estadisticas ----

#Primer activo

mean(Pc[,1]) # Promedios
prob = 0.95  # Percentiles
LSP = quantile(Pc[,1],prob)
LSP
LIP = quantile(Pc[,1],1-prob)
LIP

#Segundo activo

mean(Pc[,2]) # Promedios 
prob = 0.95  # Percentiles
LSP = quantile(Pc[,2],prob)
LSP
LIP = quantile(Pc[,2],1-prob)
LIP


#Tercer Activo

mean(Pc[,3]) # Promedios
prob = 0.95  # Percentiles
LSP = quantile(Pc[,3],prob)
LSP
LIP = quantile(Pc[,3],1-prob)
LIP


#### Graficos ----

par(mfrow = c(1,3))
plot(Pc[,1], Pc[,2])
plot(Pc[,1], Pc[,3])
plot(Pc[,3], Pc[,2])



## SIMULACION RETORNO DE UNA CARTERA ----


### INDEPENDIENTES ----

# Parametros

P0 = c(45,50,108)  
mu = c(0.10,0.15,0.08) 
sigma = c(0.20,0.25,0.30) 
T = 0.5 
m = 1000 
Q = c(100,150,120) 

# Valuo mi cartera
Q = c(100,150,120)  # Cant
V0 = Q*P0           # Valores
VI<-sum(V0)         # Valor inicial
V0
VI

z = matrix(rnorm(3*m),nrow=m,ncol=3) 
cor(z) 
P = matrix(NA,m,3)  
for (i in 1:m) { 
  for (k in 1:3) {  
    P[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*z[i,k]) 
  }
}


P0.m = matrix(rep(P0,m), m, 3 ,byrow=T)  # Matriz con P0, 'm' veces
RL = log(P/P0.m) 
cor(RL)

# Retorno de la cartera
Q.m = matrix(rep(Q,m),nrow = m,ncol = 3,byrow = T)
VT = Q.m*P       # Matriz de valores en el horizonte T de los activos
VF = matrix(rowSums(VT),m,1)    # Vectores finales

        

# Rendimiento de la cartera
RL.V = log(VF/VI) 
dev.off() # Solo si me tira error de margenes
hist(RL.V)


### CORRELACIONADOS ----

# Parametros
P0 = c(45,50,108)  
mu = c(0.10,0.15,0.08) 
sigma = c(0.20,0.25,0.30) 
T = 0.5 
m = 1000 
Q = c(100,150,120)                 #Cantidades de los activos

# Valuacion Cartera

V0 = Q*P0 
VI = sum(V0) 
V0
VI

z = matrix(rnorm(3*m), nrow=m, ncol=3) 
Rho = diag(3)  
Rho[1,2] = Rho[2,1] = 0.90 
Rho[1,3] = Rho[3,1] = 0.70 
Rho[2,3] = Rho[3,2] = 0.60
Rho


CH = chol(Rho) 
CH
t(CH)%*%CH

# Generamos e
e = z%*%CH
cor(e)

# Simulamos
Pc = matrix(NA, m, 3)
for (i in 1:m) { 
  for (k in 1:3) {
    Pc[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*e[i,k])
  }
}

P0.m = matrix(rep(P0,m),m,3,byrow=T) 
RLc = log(Pc/P0.m) 
cor(RLc) 
cor(e)  
cor(RLc)-cor(e)

# Retorno de la cartera
Q.m = matrix(rep(Q,m),
             nrow = m,ncol = 3,
             byrow = T)

VTc = Q.m*Pc   
VFc = matrix(rowSums(VTc),m,1)
RLc.V = log(VFc/VI) 
VI

hist(RLc.V) 



### Ambos Graficos ----

dev.off()
par(mfrow = c(2, 1))
tmp = hist(RL.V, xlim = c(-0.5, 1), ylim = c(0, 300), axes = F)
axis(side = 1, at = seq(-0.5, 1, by = 0.1))
axis(side = 2)
tmp = hist(RLc.V, xlim = c(-0.5, 1), ylim = c(0, 300), axes = F)
axis(side = 1, at = seq(-0.5, 1, by = 0.1))
axis(side = 2)

