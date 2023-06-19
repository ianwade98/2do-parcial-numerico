# 2 - DERIVACION 
x = c(0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11)
f = c(114.8878, 110.0817, 104.9943, 100.1919, 95.1450, 91.4054, 87.2913, 83.8146, 80.0810, 76.8462, 73.4838, 70.6687)

# 2.1 ----
Derivada_3P_PuntoExtremo<-function(n,x,f){
  h=x[2]-x[1]
  if(n==1){
    return((1/(2*h))*(-3*f[n]+4*f[n+1]-f[n+2]))
  } else if (n==2){
    return(paste("No es posible calcular f'(x_0) por este método."))
  } else {
    return((1/(2*h))*(3*f[n]-4*f[n-1]+f[n-2]))
  }
}

Derivada_3P_PuntoExtremo(2, x, f) 
Derivada_3P_PuntoExtremo(3, x, f)

#Unicamente se puede aproximar el P'(0.02) que es igual a -522.805. No se puede aproximar P'(0.01), porque
#el mètodo de 3 puntos extremo regresivo, va a obtener la estimación de las derivadas, siempre que 
#tenga los 2 puntos anteriores al que quiero estimar. Entonces, en este caso el P'(0.01) y P'(0.00)
#no se puede obtener porque no tenenmos (x0-h) y (x0 - 2h).


# 2.2 ----
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
derivada_segunda(x, f, 2)
derivada_segunda(x, f, 3)
