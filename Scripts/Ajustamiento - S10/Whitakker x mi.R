install.packages("pracma")
library('pracma')

u = read.csv("Suavizar.csv", header = T)
u = u[,"V1"] 
plot(u, col = "grey", xlab = 't', ylab = 'u(t)', 
     ylim = c(-1.5, 1.5), 
     xaxs = 'i', yaxs = 'i')

#Grafica ----
par(mfrow = c(2,3))
plot(u, col = "grey", xlab = 't', ylab = 'u(t)', 
     ylim = c(-1.5, 1.5), main = 'Datos', 
     xaxs = 'i', yaxs = 'i')

## Suavizo default ----
y_default = whittaker(u)
plot(u, col = "grey", xlab = 't', ylab = 'u(t)', 
     ylim = c(-1.5, 1.5), main = "Whitakker Default", 
     xaxs = 'i', yaxs = 'i')
lines(y_default, col="red")

## Suavizo Mucho ----
y_muysuave = whittaker(u, lambda = 10^9, d=2)
plot(u, col = "grey", xlab = 't', ylab = 'u(t)', 
     ylim = c(-1.5, 1.5), main = "Muy Suave", 
     xaxs = 'i', yaxs = 'i')
lines(y_muysuave, col = "blue")

## Bastante suave ----
y_bastantesuave = whittaker(u, lambda = 10^6, d=2)
plot(u, col = "grey", xlab = 't', ylab = 'u(t)', 
     ylim = c(-1.5, 1.5), main = "Bastante Suave", 
     xaxs = 'i', yaxs = 'i')
lines(y_bastantesuave, col = "green")

## Suave ----
y_suave = whittaker(u, lambda = 10^3, d=2)
plot(u, col = "grey", xlab = 't', ylab = 'u(t)', 
     ylim = c(-1.5, 1.5), main = "Suave", 
     xaxs = 'i', yaxs = 'i')
lines(y_suave, col = "yellow")

## Muy Fiel ----
y_muyfiel = whittaker(u, lambda = 0, d=2)
plot(u, col = "grey", xlab = 't', ylab = 'u(t)', 
     ylim = c(-1.5, 1.5), main = "Muy Fiel", 
     xaxs = 'i', yaxs = 'i')
lines(y_muyfiel, col = "black")

