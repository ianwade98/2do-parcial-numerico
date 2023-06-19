# 1) Cargamos el Algoritmos ----
Lagrange<-function(x,x_dado,f_dado){ ###
  Pol<-0
  n<-length(x_dado)
  Aux<-c(rep(1,n))
  for (k in 1:n){
    for(i in 1:n){
      if(k != i){
        Aux[k]<-Aux[k]*((x-x_dado[i])/(x_dado[k]-x_dado[i]))
      }
    }
    
    Pol<- Pol+f_dado[k]*Aux[k]
  }
  return(Pol)
}

# 2) Definimos los puntos ----
x= -1/3

x_dado <- c(-0.75,-0.5,-0.25,0)
f_dado <- c(-0.07181250,-0.02475,0.3349375,1.101)

Tabla_1 <- data.frame(x_dado,f_dado)


# 3) Resultado ----
Lagrange(x,x_dado,f_dado) 


# 5) Grafico
library(ggplot2)
xi <- seq(from=min(x_dado),to=max(x_dado),length.out=1000)
Px <- rep(NA,1000)
for(i in 1:1000){
  Px[i]<-Lagrange(xi[i],x_dado,f_dado)
}

DF_Lagrange <- data.frame(xi,Px)

Table_Graf2 <-merge(DF_Lagrange, Tabla_1, by.x =  "xi",by.y = "x_dado", all =T)

Graf_2 <- ggplot(data = Table_Graf2,aes(x = xi, y = Px))+
  geom_line(colour = "darkgreen", size = 1)+
  geom_point(aes(xi,f_dado))+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  ggtitle("Polinomio de Lagrange")+
  xlab("Eje x")+ 
  ylab("Eje y")+
  theme_light()

Graf_2
