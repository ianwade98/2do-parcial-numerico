# Semana 12 - Integracion 

# Integracion Numerica Simple
## Regla del trapecio: n = 1  ---- 
Integral_ReglaTrapecio<-function(x,f){
  h=x[2]-x[1]
  return((h/2)*(f(x[1])+f(x[2])))
}

## Regla de Simpson: n = 2 ----
Integral_ReglaSimpson<-function(x,f){
  h=(x[2]-x[1])/2
  return((h/3)*(f(x[1])+4*f(x[1]+h)+f(x[1]+2*h)))
}

## Regla de 3 Octavos de Simpson: n = 3 ----
Integral_ReglaSimpson_TresOctavos<-function(x,f){
  h=(max(x)-min(x))/3
  return(((3*h)/8)*(f(x[1])+3*f(x[1]+h)+3*f(x[1]+2*h)+f(x[1]+3*h)))
}

## Regla de Newton-Cotes Cerrada: n = 4 ----
IntegralNC_Cerrada<-function(x,f){
  h=(max(x)-min(x))/4
  return(((2*h)/45)*(7*f(x[1])+32*f(x[1]+h)+12*f(x[1]+2*h)+32*f(x[1]+3*h)+7*f(x[1]+4*h)))
}

## Regla de Newton-Cotes Abierta ----
IntegralNC_Abierta<-function(x,f,n){
  a=min(x)
  b=max(x)
  h=(b-a)/(n+2)
  if(n==0){
    return(2*h*f(x[1]+h))
  } else if(n==1){
    return((3*h/2)*(f(x[1]+h)+f(x[1]+2*h)))
  } else if(n==2){
    return((4*h/3)*(2*f(x[1]+h)-f(x[1]+2*h)+2*f(x[1]+3*h)))
  } else if (n==3){
    return((5*h/24)*(11*f(x[1]+h)+f(x[1]+2*h)+f(x[1]+3*h)+11*f(x[1]+4*h)))
  }
}

# Integracion Numerica Compuesta
## Regla Compuesta de Simpson ----
ReglaCompuestaSimpson<-function(a,b,n,f){
  h=(b-a)/n
  I0=f(a)+f(b)
  I1=0
  I2=0
  x=0
  for(i in 1:(n-1)){
    x=a+i*h
    if(i%%2==0){
      I2=I2+f(x)
    }else{
      I1=I1+f(x)
    }
  }
  return(h*(I0+2*I2+4*I1)/3)
}

## Regla Compuesta del Trapecio ----
ReglaCompuestaTrapezoidal<-function(a,b,n,f){
  h=(b-a)/n
  I1=0
  for(i in 1:(n-1)){
    I1=I1+f(a+i*h)
  }
  return(h/2*(f(a)+2*I1+f(b)))
}

## Regla Compuesta del Punto Medio ----
ReglaCompuestaPuntoMedio<-function(a,b,n,f){
  if(n%%2==0){
    h=(b-a)/(n+2)
    I=0
    for(i in -1:(n+1)){
      x=a+(i+1)*h
      if(i%%2==0){
        I=I+f(x)
      }
    }
    return(2*h*I)
  } else {
    return(paste("El parámetro n debe ser par"))
  }
}
