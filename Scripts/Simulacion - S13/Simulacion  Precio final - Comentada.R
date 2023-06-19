rm(list = ls())
graphics.off()

# SIMULACION DEL PRECIO DE UN ACTIVO FINANCIERO
## MODELO GEOMETRICO BROWNEANO

### PRECIO FINAL SIN CAMINO DE PRECIOS

#Parámetros

P0 = 45
mu = 0.10       # Retorno esperado
sigma = 0.20    #volatilidad
T = 0.5         #seis meses (año seria 1)
n = 182         #número de time-steps 
dt = T/n        #tamaño de cada time-step (delta t) - Aprox 1 dia cada delta t


#Simulación Precio Final

m = 1000
e = rnorm(m) #creo la matriz de precios de epsilon de una vez.
PT = matrix(NA,nrow = m, ncol = 1) #Matrix (vector) de precios finales
PT = P0 * exp((mu - 0.5*sigma^2)*T + sigma * sqrt(T) * e) # ecuacion 
hist(PT)

#Métricas Estadísticas
#Promedios
mean(PT)

#Percentiles
prob = 0.95

LSPT = quantile(PT,prob)
LSPT

LIPT = quantile(PT,1-prob)
LIPT

