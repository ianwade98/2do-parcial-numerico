# 2 - Derivacion 
x = c(0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11, 0.12)
f = c(115.3371, 109.7031, 104.8099, 100.0653, 95.3516, 91.1998, 87.2676, 83.5825, 79.9116, 76.8435, 73.9473, 70.0320, 67.7022)

# 2.1
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

Derivada_3P_PuntoExtremo(5, x, f)
Derivada_3P_PuntoExtremo(4, x, f)

# 2.2
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

derivada_segunda(x, f, 4)
derivada_segunda(x, f, 5)