rm(list=ls())

## Metodo de Euler ----
fn = function(t,y){     
  return(t-y+2)
}

# Algoritmo ----

Metodo_Euler = function(a, b, N, alfa){ # alfa = y0
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

Metodo_E = Metodo_Euler(0, 1, 10, 2)

# Grafico ---- 
# NO FUNCIONA
Data = Metodo_E
plot(data = Metodo_E, aes(x = "t", y = "w", type = "b"), 
     ylim = c(0, 6), ylab = "w", xlab = "t", 
     xaxs = 'i', yaxs = 'i')
lines(t_exacta, y_exacta, col = "red")
legend('topleft', legend = c("Exacta", "Euler N=10"), 
       col = c("red", "black"), 
       lty = c(1, 1), pch = c(NA, 1))
title("Solucion del problema y'= y - t^2 + 1")

# Grafico 2 ----
Grafico_Euler = ggplot(data = Metodo_E,
                       aes(x = t, y = w))+
  geom_point(size=1, color="blue")+ 
  geom_vline(xintercept=0)+
  geom_hline(yintercept=0)+
  scale_x_continuous(name = "t", breaks = seq(0, 2, by = 0.5)) +
  scale_y_continuous(name = "w", breaks = seq(0, 6, by = 0.5))
ggtitle("Metodo Euler")


Grafico_Euler

## Metodo de Runge Kutta ----
fn <- function(t,y){     # Completar con la función f(t,y(t)) que es igual a dy/dt
  return(-20*y + 7 *exp(-1/2*t) )
}
# Algoritmo ----
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

# Grafico ----

## Grafico de ambos juntos ----

# Datos
Metodo_RK = runge_kutta(0, 0.1, 10, 5)
Metodo_E = Metodo_Euler(0, 0.1, 10, 5)

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