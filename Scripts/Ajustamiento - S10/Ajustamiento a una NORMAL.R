# Ajustamiento a una normal

x_dado <- c(0.0227,0.0817,0.3147,0.5258,0.7502,0.8877,1.3583,1.3716,1.5854,1.6288,2.8558,3.0106,5.1854)
y_dado <- c(0.0769,0.1538,0.2308,0.3077,0.3846,0.4615,0.5385,0.6154,0.6923,0.7692,0.8462,0.9231,1)
data = data.frame(x_dado,y_dado)  
data
media = mean(data$x_dado)
desvioestandar = sd(data$x_dado)



Modelo = nls(y_dado ~ pnorm(x_dado,mu,sigma), 
             data = data,
             start = list(mu = media, sigma = desvioestandar))
Modelo

Aj = sapply(0.0522, function(x) pnorm(x,mean = 1.092, sd = 1.044)) 
Aj