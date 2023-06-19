DiferenciasDivididas<-function(x,y){
  
  n = length(x)
  DifDiv <- matrix(NA, nrow = n, ncol = n)
  DifDiv[,1] <- y
  Coef <- rep(NA,n)
  
  Coef[1]<- DifDiv[1,1]
  
  for(i in 1:(n-1)){
    for(j in 1:i){
      DifDiv[(i+1), (j+1)] <- (DifDiv[(i+1),j] - DifDiv[i,j])/(x[i+1] - x[i+1-j])
      if(i+1==j+1){
        Coef[i+1] <-DifDiv[(i+1),(j+1)]
      }
    }
  }
  print(paste("Tabla de diferencias:"))
  print(DifDiv)
  print(paste("Coeficientes:"))
  print(Coef)
  return(Coef)
}

x_dado=c(1,1.3,1.6,1.9,2.2)
f_dado=c(0.7651977,0.6200860,0.4554022,0.2818186,0.1103623)

options(scipen = 100, digits= 7)
DiferenciasDivididas(x_dado,f_dado)

Coeficientes<-DiferenciasDivididas(x_dado,f_dado)

PolinomioNewton<-function(coef,x_dado){
  P=paste(coef[1])
  aux=""
  n=length(x)
  for(i in 2:n){
    for(j in (i-1):1){
      aux=paste(aux,"(x-",x[j],")")
    }
    P=paste(P,"+", "(",coef[i],")",aux)
    aux=""
  }
  return(paste("P(x)=",P))
}

PolinomioNewton(Coeficientes,x_dado)

Interpolacion_Newton<-function(x0,x,y,coeff){
  aux=0
  n=length(y)
  aux=coeff[1]
  aux2=1
  for(i in 2:n){
    for(j in (i-1):1){
      aux2=aux2*(x0-x[j])
    }
    aux=aux+coeff[i]*aux2
    aux2=1
  }
  return(aux)
}

Interpolacion_Newton(1.5,x_dado,f_dado,Coeficientes)
