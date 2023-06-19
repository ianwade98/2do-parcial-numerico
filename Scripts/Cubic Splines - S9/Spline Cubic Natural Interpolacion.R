# DATOS ----
x=c(-0.5,-0.25,0.25,0.5)
fx=c(1.93750,1.33203,0.800781,0.6875)
A=cbind(x,fx)

#INTERPOLO EN UN PUNTO----
#Funcion para que solo interpole y no grafique
Interpolo_en_pto = function(A,xk){
  #Variables iniciales 
  x = A[,1]
  fx = A[,2]
  if(length(x)!=length(fx)){
    return("La cantidad de argumentos no coincide")
  }
  n = length(A[,1]); b = c(rep(NA,n)); c = c(rep(NA,n)); d = c(rep(NA,n)); h = c(rep(NA,n))
  a = c(rep(NA,n)); l = c(rep(NA,n)); u = c(rep(NA,n)); z = c(rep(NA,n))
  #Paso 1 y 2 ----
  for(i in 1:(n-1)){
    h[i] = x[i+1]-x[i]
  }
  for(i in 2:(n-1)){
    a[i] = ((3/(h[i])) * (fx[i+1]- fx[i])) - ((3/(h[i-1])) * (fx[i]- fx[i-1]))
  }
  #Paso 3 ----
  l[1] = 1;u[1] = 0;z[1] = 0
  #Paso 4 ----
  for(i in 2:(n-1)){
    l[i] = 2*(x[i+1] - x[i-1]) - (h[i-1]*u[i-1])
    u[i] = h[i]/l[i]
    z[i] = (a[i]- h[i-1]*z[i-1])/l[i] 
  }
  #Paso 5 ----
  l[n] = 1 ;z[n] = 0;c[n] = 0
  #Paso 6 ----
  for(j in (n-1):1){
    c[j] = z[j] - (u[j]*c[j+1])
    b[j] = ((fx[j+1] - fx[j])/h[j]) - h[j]*(c[j+1] + 2*c[j])/3
    d[j] = (c[j+1] - c[j])/(3*h[j])
  }
  #interpolacion en el punto ----
  for(i in 1:(n-1)) {
    if(x[i]<= xk & x[i+1]>= xk){
      s = i}
  } #Nos fijamos a que polinomio pertenece el valor a interpolar
  yk = fx[s]+b[s]*(xk-x[s])+c[s]*(xk-x[s])^2+d[s]*(xk-x[s])^3 #Lo valuamos en el polinomio correspondiente
  return(yk)
}

Interpolo_en_pto(A, 0)