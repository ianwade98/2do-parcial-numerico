# INTEGRACION GRAFICO
mu <- 3.65
sigma <- 0.18
x1 <- seq(from = 0.0001, to = 100, by = 0.1)
fx <- eval(expression( ((1)/(x*sigma*sqrt(2*pi))) * exp(- (log(x)-mu)^2/(2*sigma^2)) ), list(x = x1))
ggplot() +
  geom_line(aes(x = x1, y = fx))

# Y ahi me fijo los puntos para buscar la esperanza
# Para derivada sobre algo: 
# Escribo de vuelta mi funcion 
# mu = mu + limite inferior de esperanza (0.00001)
# fx + h = mi funcion nuevamente

# (fx_h - fx) / (10^-10)

