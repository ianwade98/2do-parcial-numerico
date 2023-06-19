# DERIVADAS SEGUNDAS

# El que me paso Flor
derivada_segunda = function(x, y, xindice){
  h = diff(x)[1]
  n = length(y)
  yprima = c(rep(NA,n))
  
  if(missing(xindice)){
    for (i in 2:(n - 1)) {
      yprima[i] = (y[i-1] - 2*y[i] + y[i+1])/(h^2)
    }
    
    tabla = cbind.data.frame(x, y, yprima)
    return(tabla)
  }
  else{
    if(xindice == 1 || xindice == n){
      return("Este valor no se puede calcular")
    }
    else{
      yprima[xindice] = (y[xindice-1] - 2*y[xindice] + y[xindice+1])/(h^2)
      return(yprima[xindice])
    }
  }
}

# El del recu, solo PM
#2da DERIVADA ####
#FORMULA DEL PUNTO MEDIO PARA LA SEGUNDA DERIVADA (3 PUNTOS) ####
D_segunda3_medio = function(x1,h){
  derivada = (1/(h^2))*(f(x0-h)-2*f(x0)+f(x0+h))
  return(derivada)
}

D_segunda3_midpoint = function(x,fx,x0,h){
  for(j in 1:length(x)){
    if(x[j]==x0){
      i = j
      break
    }
  }
  derivada = (1/(h^2))*(fx[i-1]-2*fx[i]+fx[i+1])
  return(derivada)
}

D_segunda3_midpoint(x,fx,3,0.1)