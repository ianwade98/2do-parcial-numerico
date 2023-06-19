rm(list = ls())
graphics.off()

##################SIMULACION DE PRECIOS DE ACTIVOS FINANCIEROS CORRELACIONADOS O NO
####MODELO GEOMETRICO BROWNEANO
###SIMULACION CAMINO DE PRECIOS PARA 3 ACTIVOS (no hacemos camino, sino que vamos de 0 a T)

#ACTIVOS CORRELACIONADOS ENTRE SI

P0<-c(45,50,108)  #vector de los 3 precios iniciales
mu<-c(0.10,0.15,0.08) #rendimientos esperados para 3 activos
sigma<-c(0.20,0.25,0.30) #volatilidad para 3 activos
T<-0.5 #seis meses
m<-1000 #cantidad de camino de precios

z<-matrix(rnorm(3*m),nrow=m,ncol=3) #Matriz de numeros aleatorios con distribución N(0,1)
#Como en este caso tengo 3 activos, genero 3000 (3  cant de activo y m cant de pcio final)


#Genero la matríz Rho, la diag ppal tiene uno y fuera :
Rho<-diag(3)  #genera matriz identidad
Rho[1,2]<-Rho[2,1]<-0.90 #Corr entre el rendimineto del act 1 y 2 
Rho[1,3]<-Rho[3,1]<-0.70 # corr entre el rend del act 1 y 3
Rho[2,3]<-Rho[3,2]<-0.60 #corr entre el rend del activo 2 y 3

Rho

#Calculo Cholesky

CH<-chol(Rho) #calculo la factorización de choleky de la matriz de correlaciones
CH

t(CH)%*%CH  #como R brinda una matriz triangular SUPERIOR primero multiplcio la traspuesta para verificar

#generamos matriz "e" que tiene numeros epsilon correlacionados

e<-z%*%CH

cor(e) #correalción de los números aleatorios con distr N(0,1). Vemos que estan correlacionados
#la correlación de "e" debería ser muy parecida a Rho, por el error muestral no es exactamente igual


##A partir de los epsilon correlacionados que se crearon, se simulan los precios
#correlaciones


Pc<-matrix(NA,m,3) #Se crea matriz de precios correlacionados con "m" 
#(número de simulaciones) filas 
#y 3 (activos) columnas

for (i in 1:m) { #Bucle que cuenta el número de simulación
  for (k in 1:3) {  #Bucle que calcula el precio del activo 'k' en la simualción 'i'
    Pc[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*e[i,k])
  }
}

P0.m<-matrix(rep(P0,m),m,3,byrow=T)  #creo una matriz con vector P0, repetido 'm' veces
RLc<-log(Pc/P0.m) #calculo rendimientos logarítmicos de los activos

cor(RLc) #tiene que ser igual a cor(e)

cor(e)  #verifico
cor(RLc)-cor(e)


#Para métricas estadísticas fijarse en precios no correlacionados

  #Gráficos
  par(mfrow=c(1,3))
  plot(Pc[,1],Pc[,2])
  plot(Pc[,1],Pc[,3])
  plot(Pc[,3],Pc[,2])


##SIMULACION RETORNO DE UNA CARTERA PRECIOS CORRELACIONADOS

# 1) DATOS:
  P0 = c(45,50,108)  # Precios iniciales
  mu = c(0.10,0.15,0.08) # Rendimientos esperados
  sigma = c(0.20,0.25,0.30) # Volatilidad para 3 activos
  T = 0.5 #
  m = 1000 
 Q = c(100,150,120) # Cantidad cada activo
 
 # Valuo mi cartera
 
V0 = Q*P0 #Valor de cada activo
VI = sum(V0) #Valor inicial de la cartera
V0
VI

z = matrix(rnorm(3*m), nrow=m, ncol=3) 

#Genero la matríz Rho, la diag ppal tiene uno y fuera :
Rho = diag(3)  #genera matriz identidad
Rho[1,2] = Rho[2,1] = 0.90 #Corr entre el rendimineto del act 1 y 2 
Rho[1,3] = Rho[3,1] = 0.70 # corr entre el rend del act 1 y 3
Rho[2,3] = Rho[3,2] = 0.60 #corr entre el rend del activo 2 y 3
Rho

CH = chol(Rho) # Calculo la factorización de choleky de la matriz de correlaciones
CH
t(CH)%*%CH  # Como R brinda una matriz triangular SUPERIOR primero multiplcio la traspuesta para verificar

#generamos matriz "e" que tiene numeros epsilon correlacionados
e = z%*%CH
cor(e)

#2.5
Pc = matrix(NA,m,3) #Se crea matriz de precios correlacionados 

for (i in 1:m) { #Bucle que cuenta el número de simulación
  for (k in 1:3) {  #Bucle que calcula el precio del activo 'k' en la simualción 'i'
    Pc[i,k]<-P0[k]*exp((mu[k]-0.5*sigma[k]^2)*T+sigma[k]*sqrt(T)*e[i,k])
  }
}


P0.m = matrix(rep(P0,m),m,3,byrow=T)  #creo una matriz con vector P0, repetido 'm' veces
RLc = log(Pc/P0.m) #calculo rendimientos logarítmicos de los activos
cor(RLc) 
cor(e)  #verifico que sea igual a cor(RLc)
cor(RLc)-cor(e)

# Retorno de la cartera
Q.m = matrix(rep(Q,m),
            nrow = m,ncol = 3,
            byrow = T)
Q.m

# Mult la matriz generada q.m por el vetor de precios aleatorios generado
VTc = Q.m*Pc   #Matriz de valores en el horizonte T de los activos
VFc = matrix(rowSums(VTc),m,1)
RLc.V = log(VFc/VI) #Calculo rendimiento de la cartera
hist(RLc.V)  #mayor dispersión que cuando comparas activos sin correlación
VI
RLc.V
