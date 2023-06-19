rm(list=ls())

x_dado<-c(2,2.75,4)
x=3
Fx<-function(x){
  return(1/x)
}
fx<-Fx(x)

#Calcula coeficientes de interpolación de Lagrange ----
PILagrange<-function(x,x_dado){
  n=length(x_dado)
  L=c(rep(1,n))
  for(k in 1:n){
    for(i in 1:n){
      if(k!=i){
      L[k]<-L[k]*((x-x_dado[i])/(x_dado[k]-x_dado[i]))
      }
    }
  }
  return(L)
}

#Calcula interpolación de Lagrange ----
InterpolacionLagrange<-function(x,f_dado,L){
  n=length(x)
  interpolacion=0
  for(i in 1:n){
    interpolacion=interpolacion+f_dado[i]*L[i]
  }
  return(interpolacion)
}

#Imprime polinomio interpolante ----
ImprimePILagrange<-function(coef,y_dado){
  P=paste0(coef[1])
  #Primer término
  aux1=coef[1]*y_dado
  polinomio=paste0(aux)
  n=length(coef)
  for(i in 2:n){
    for(j in (i-1):1){
      aux=paste0(aux,"(x-",x[j],")")
    }
    P=paste0(P,"(",coef[i],")",aux,"-")
    aux=""
  }
  return(P)
}

L<-PILagrange(x,x_dado,Fx)
InterpolacionLagrange(Fx,x_dado,L)

#----
x2_dado=c(0,0.25,0.5,0.75)
f2_dado=c(1,1.64872,2.71828,4.48169)
PILagrange(0.43,x2_dado)
InterpolacionLagrange(x2_dado,f2_dado,PILagrange(0.43,x2_dado))
ImprimePILagrange(x2_dado,f2_dado)

PILagrange(0,x3_dado)
InterpolacionLagrange(x3_dado,f3_dado,PILagrange(0,x3_dado))
