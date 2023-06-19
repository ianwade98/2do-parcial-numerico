# 5. Ecuaciones Diferenciales
fn = function(t,y){     
  return(cos(y)/t^1.69 + t/y^3)
}

# a. ----
Metodo_Euler = function(a, b, N, alfa){ 
  h = (b - a)/N
  w = c(rep(NA, N + 1))
  w[1] = alfa
  t = c(seq(a, b, h))
  t[1] = a
  for (i in 1:N) {
    w[i + 1] = w[i] + h * fn(t[i], w[i])
  }
  return(data.frame(t,w))
}

Metodo_Euler(4, 5, 45, 0.46)
Metodo_E = Metodo_Euler(4, 5, 45, 0.46)


# b. ----
runge_kutta = function(a, b, N, alfa){
  h = (b - a)/N
  w = c(rep(NA, N + 1))
  w[1] = alfa
  t = c(seq(a, b, h))
  t[1] = a
  for (i in 1:N) {
    k1 = h*fn(t[i], w[i])
    k2 = h*fn(t[i] + h/2, w[i] + k1/2)
    k3 = h*fn(t[i] + h/2, w[i] + k2/2)
    k4 = h*fn(t[i + 1], w[i] + k3)
    w[i + 1] = w[i] + (k1 + 2*k2 + 2*k3 + k4)/6
    
  }
  return(data.frame(t,w))
}
runge_kutta(4, 5, 46, 0.46)
Metodo_RK = runge_kutta(4, 5, 46, 0.46)
# c ----
library(ggplot2)
Metodo_E = Metodo_Euler(4, 5, 45, 0.46)
Metodo_RK = runge_kutta(4, 5, 46, 0.46)
DF_Graf = data.frame(Metodo_RK$t, Metodo_RK$w, Metodo_E$w)
colnames(DF_Graf) <- c("t","Runge_Kutta","Euler")
colors <- c("Runge_Kutta" = "#69b3a2", "Euler" = "pink")

#Grafico 

Grafico_RungeKutta_Euler <-ggplot(data = DF_Graf,
                                  aes(x = t,))+
  geom_point(aes(y = Runge_Kutta, color = "Runge_Kutta"),size=1)+ 
  geom_point(aes(y = Euler, color = "Euler"), size = 1)+
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  ggtitle("Metodo Runge Kutta & Euler")+ 
  xlab("t")+ 
  ylab("y")+
  labs(colour = "Legend")+
  scale_color_manual(values = colors)+
  theme_classic()

Grafico_RungeKutta_Euler

# d. ---- 
#Los métodos de Taylor de orden superior (como Euler) tienen error de truncamiento de orden alto, lo que hace que aproximen mejor,
# pero es necesario conocer las derivadas. En cambio, el método de Runge Kutta, lo hace mas "económico" porque no es necesario conocer las derivadas
# pero esto lo hace menos exacto.