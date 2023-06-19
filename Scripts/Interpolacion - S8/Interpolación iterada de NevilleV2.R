#Interpolación iterada de Neville
Metodo_Neville = function (x, y, x0){
  n = length(x)
  q = matrix(data = NA, n, n)
  q [ ,1] = y
  #Paso 1
  for (i in 2:n){
    for (j in i:n){
      q[j,i] <- (((x0 - x[j-i+1]) * q[j,i-1] - (x0 - x[j]) * q[j-1,i-1])) / (x[j] - x[j-i+1])
    }
  }
  Resultados = list("Valor aproximado" = q[n, n], "Tabla iterada" = q)
  return(Resultados)
  return(q[n, n])
}
#Cargo valores
x = c(0, 0.25, 0.5, 0.75)
y = c(1, 1.64872, 2.71828, 4.48169)
x0 = 0.43
#Resultados
Metodo_Neville (x, y, x0)
