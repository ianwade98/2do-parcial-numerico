#Polinomio interpolante de Lagrange
#Esto sirve si no me pide los polinomios de distintos grados. 

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

#Definiendo valores
x = -1/3
x_dado = c(-0.75, -0.5, -0.25, 0)
y_dado = c(-0.0781250, -0.02475, 0.3349375, 1.101)

#Resultados
Coef_Lagrange (x, x_dado)
Interp_Lagrange (x, x_dado, y_dado)

  #Gráfico del polinomio de Lagrange
plot (x_dado, y_dado, xlab = "x", ylab = "f(x)", type = "b")
points (x, Interp_Lagrange (x, x_dado, y_dado), col = "darkgreen", pch = 17)
xi = seq(from = min(x_dado), to = max(x_dado), length.out = 1000)
Px = rep(NA, 1000)
for (i in 1:1000){
  Px[i] = Interp_Lagrange (xi[i], x_dado, y_dado)
}
lines (xi, Px, col = "red")


