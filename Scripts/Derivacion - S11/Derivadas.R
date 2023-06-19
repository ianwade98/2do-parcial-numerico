#Scripts Derivadas

# Derivada Progresiva ----

Derivada_DifProgresiva<-function(f1,f2,h){
  return((f2-f1)/h)
}

# Derivada Regresiva ----
Derivada_DifRegresiva<-function(f1,f2,h){
  return((f1-f2)/h)
}

# Derivada de tres puntos : Punto Medio ----
Derivada_3P_PuntoMedio<-function(n,x,f){
  h=x[2]-x[1]
  return((1/(2*h))*(-f[n-1]+f[n+1]))
}
# Derivada de tres puntos : Punto Extremo ----
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

# Derivada de tres Puntos - Devuelve un vector con los cálculos de todas las derivadas ----
Derivada_3P<-function(x,f){
  
  n=length(x)
  h=x[2]-x[1]
  derivadas=rep(NA,n)
  
  for (i in 1:n){
    if (i==1){
      derivadas[i]=((1/(2*h))*(-3*f[i]+4*f[i+1]-f[i+2]))
    } else if (i>1 & i<n){
      derivadas[i]=(((1/(2*h))*(-f[i-1]+f[i+1])))
    } else{
      derivadas[i]=((1/(2*h))*(3*f[i]-4*f[i-1]+f[i-2]))
    }
  }
  
  return(derivadas)
}

# Derivada de cinco puntos: Punto Medio ----
Derivada_5P_PM<-function(x,fx,n){
  h=x[2]-x[1]
  return((1/(12*h))*(fx[n-2]-8*fx[n-1]+8*fx[n+1]-fx[n+2]))
}

# Derivadas de cinco puntos: Puntos Extremos ----
Derivada_5P_ExtremoI<-function(x,fx,n){
  h=x[2]-x[1]
  return((1/(12*h))*(-25*fx[n]+48*fx[n+1]-36*fx[n+2]+16*fx[n+3]-3*fx[n+4]))
}

Derivada_5P_ExtremoS<-function(x,fx,n){
  h=x[2]-x[1]
  return((1/(12*h))*(+25*fx[n]-48*fx[n-1]+36*fx[n-2]-16*fx[n-3]+3*fx[n-4]))
}


# Derivada Segunda ----
h = diff(x)[1]
Derivada_segunda = function(x, fx, n, h){
  m = length(fx)
  fx_prima
  
  if(missing(n)){
    for (i in 2:(n - 1)) {
      fx_prima[i] = (fx[i-1] - 2*fx[i] + fx[i+1])/(h^2)
    }
    
    tabla = cbind.data.frame(x, y, fx_prima)
    return(tabla)
  }
  else{
    if(n == 1 || n == m){
      return("Este valor no se puede calcular")
    }
    else{
      fx_prima[n] = (fx[n-1] - 2*fx[n] + fx[n+1])/(h^2)
      return(fx_prima[n])
    }
  }
}
