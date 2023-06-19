rm(list=ls())
graphics.off()

#Neville ----
# 1) Cargamos los datos ----
x <- c(1,1.3,1.6,1.9, 2.2)
y <- c(0.7651977, 0.6200860, 0.4554022, 0.2818186, 0.1103623)

TablaInicial<-data.frame(x,y)

# 2) Algoritmo de Neville ----
Neville_3.1 <- function(x, y, x0) {
  
  n <- length(x)
  q <- matrix(data = 0, n, n)
  q[,1] <- y
  
  #Paso 1
  
  for (i in 2:n) {
    for (j in i:n) {
      q[j,i] <- (((x0 - x[j-i+1]) * q[j,i-1] - (x0 - x[j]) * q[j-1,i-1])) / (x[j] - x[j-i+1])
    }
  }
  
  #Paso 2
  
  res <- list('Valor aproximado'=q[n,n], 'Tabla de Iteraciones'=q)
  return(res)
  
  return(q[n,n])
}
# 3) Resolucion ----
options(scipen = 100, digits= 5)
Neville_3.1(x,y,1.5)

