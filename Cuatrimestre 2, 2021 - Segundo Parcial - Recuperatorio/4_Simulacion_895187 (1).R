#Facundo Matias De Lorenzo

#Registro: 895187

#EJERCICIO 4: SIMULACIÓN DE MONTECARLO

#4.1 Camino de precios----
set.seed(895187)

P0 = 96
mu = 0.19
sigma = 0.15
T = 6/12
n = 30.25*6 
dt = (6/12)/(30.25*6) 
m = 1941

#Simulacion camino de precios 
Pt = matrix(NA,nrow = m, ncol = n+1)
Pt [,1] = P0 
for (i in 1:m){ 
  for (t in 2:(n+1)) { 
    Pt[i,t] = Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1)) 
  }
}


k=length(Pt[1,])
P = mean(Pt[,k])
Desvio = sd(Pt[,k])
P #105.191
Desvio #11.19822
hist(Pt)

#Precios finales
e = rnorm(m)

PT = matrix (NA,nrow = m , ncol = 1)
PT = P0*exp((mu-0.5*sigma^2)*T + sigma*sqrt(T)*e)
hist(PT, main = "Simulacion del precio final",xlab = "Precios simulados", ylab = "Frecuencia")

P = mean(PT)
Desvio = sd(PT)
P #105.7745
Desvio #11.16131



#4.2 Probabilidad----

sum(Pt[,n+1] > mean(Pt[,n+1]))/m
#0.4827409


#4.3 Percentil----
prob <-0.95

quantile(PT,prob) #125.4278 

quantile(PT,1-prob) #RESPUETA: 88.31678 

