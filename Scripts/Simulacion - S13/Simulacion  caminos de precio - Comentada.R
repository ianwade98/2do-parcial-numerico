rm(list = ls())
graphics.off()

# SIMULACION DEL PRECIO DE UN ACTIVO FINANCIERO 

## MODELO GEOMETRICO BROWNEANO

### CAMINO DE PRECIOS ----

# Supuesto de MGB - Parametros 

P0 = 45
mu = 0.10       # Retorno esperado
sigma = 0.20    #volatilidad
T = 0.5         #seis meses (año seria 1)
n = 182         #número de time-steps 
dt = T/n        #tamaño de cada time-step (delta t) - Aprox 1 dia cada delta t

#Simulación de Caminos de Precios
m = 1000       # Cantidad de caminos de precios (simulaciones)
Pt = matrix(NA, nrow = m, ncol = n+1) #matriz de camino de precios con m filas y n+1 columnas (P0 + 182)

Pt[,1] = P0    # 1era columna de todas las filas

for (i in 1:m) {              #bucle simulaciones
  for (t in 2:(n+1)) {        #el 2 porque arrance de la fila 2 porque 1 era P0 (precio inicial)
    Pt[i,t] = Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
  } #el precio en el momento T es igual al precio en t-1 : y toda la formula
} #Para generar el epsilon de la formula hago Rnorm que genera un numero aleatorio (uno), porque
#quiero solo UN numero aleatorio


### Gráficos ----

t = rep(0:n, m)   #crea un vector de '0' a 'n' y lo repito 'm' veces
t = matrix(t,nrow = m,ncol = n+1,byrow = T) #Ordena el vector anterior en una matriz
#primer fila y barro todas las columnas del vector tiempo, y 
#luego primer file ay barro col del ector precios
plot(t[1,], Pt[1,], type="l", ylim = c(min(Pt),max(Pt))) #grafica el primer camino
for (i in 2:m) {
  lines(t[i,], Pt[i,], col = trunc(runif(1)*m))
}

#Métricas Estadísticas

#Vector de Promedios de cada uno de los momentos 
M = matrix(NA, nrow = 1, ncol = n+1)
for (i in 1:(n+1)) {
  M[i] = mean(Pt[,i]) #la media por cada una de las columnas
}

#Vector de Percentiles
prob<-0.95   #estimación de intervalos de confianza de los precios para cada momento al 90% (porque deja 5% arriba y 5% abajo)

#limite superior que acumula ese valor
LS = matrix(NA, nrow = 1, ncol = n+1)
for (i in 1:(n+1)) {
  LS[i] = quantile(Pt[,i], prob) #la funcion quantil de R para calc percentiles
}
#limite inferior que acumula ese valor
LI = matrix(NA, nrow = 1, ncol=n+1)
for (i in 1:(n+1)) {
  LI[i] = quantile(Pt[,i],1-prob)
}

#Agrego al grafico, con lines, la media y perc.

#Grafico del limite superior
lines(t[1,], LS, col = 'red',lwd = 5) #line width(lwd)
#Grafico del limite inferior
lines(t[1,], LI, col = 'red',lwd = 5) #line width(lwd)
#grafico de la media
lines(t[1,], M, col = 'black',lwd = 5)#line width(lwd)

LS[183] # valor final del limite superior del intervalo de confianza
M[183] #valor final de promedio
LI[183] # valor final del limite inferior del intervalo de confianza

