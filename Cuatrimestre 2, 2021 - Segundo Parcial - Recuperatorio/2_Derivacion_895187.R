#Facundo Matias De Lorenzo

#Registro: 895187

#EJERCICIO 2: DERIVACÓN

r <- seq(0,0.11, by = 0.01)
P <- c(114.8878,110.0817,104.9943,100.1919,95.1450,91.4054,87.2913,83.8146,80.0810,76.8462,73.4838,70.6687)
data = data.frame(r,P)
data

#2.1 deriviada primera 

Punto_Extremo_3_Reg <- function(x,f){
  n<-length(x)
  Punto_Extremo_3_Reg = rep(NA,n)
  h = x[1]-x[2]
  for (i in 3:n) {
    Punto_Extremo_3_Reg[i]<-(1/(2*h))*(-3*f[i]+4*f[i-1]-f[i-2])  
  }
  return(data.frame(x,f,Punto_Extremo_3_Reg))
}
Punto_Extremo_3_Reg(r,P)

#Unicamente se puede aproximar el P'(0.02) que es igual a -522.805. No se puede aproximar P'(0.01), porque
#el mètodo de 3 puntos extremo regresivo, va a obtener la estimación de las derivadas, siempre que 
#tenga los 2 puntos anteriores al que quiero estimar. Entonces, en este caso el P'(0.01) y P'(0.00)
#no se puede obtener porque no tenenmos (x0-h) y (x0 - 2h).

#2.2 derivada segunda

Derivada_Segunda_PM <- function(x,f){
  n<-length(x)
  Derivada_Segunda_PM = rep(NA,n)
  h = x[2]-x[1]
  for (i in 2:n) {
    Derivada_Segunda_PM[i]<-(1/(h^2))*(f[i-1]-2*f[i]+f[i+1])
  }
  return(data.frame(x,f,Derivada_Segunda_PM))
}
Derivada_Segunda_PM(r,P)

#En este caso, unicamente se pudo obtener la derivada segunda de ambos puntos
#P'(0.01) = -2813
Derivada_Segunda_PM(r,P)[2,3]
#  P'(0.02) = 2850
Derivada_Segunda_PM(r,P)[3,3]




