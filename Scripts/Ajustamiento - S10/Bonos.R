rm(list = ls())
graphics.off()

## Ejemplo curva de rendimientos USD BONDS ----
Datos = read.csv("C:/Users/flore/OneDrive/Desktop/Datos.csv", header = T, row.names = 1)

plot(Datos)
abline(h=c(0,1), col = 'lightgrey')
Modelo = nls(Fx~pnorm(x,mu,sigma),
             data = Datos,
             start = list(mu = 120, sigma = 50))

Summary(Modelo)

ajusteNL = data.frame(x = seq (from=min(Datos$x), to = max(Datos$x),
                               length.out = 1000))
ajusteNL$fit = predict(Modelo, newdata = list (x = ajusteNL$x))
lines(ajusteNL$x, ajusteNL$fit, col ='red', lty = 'dashed')

mu_muestra = mean(Datos$x)
sigma_muestra = sd(Datos$x)

curve(pnorm(x,mu_muestra,sigma_muestra), col = 'blue', add = T,
      from = min(Datos$x), to = max(Datos$x))

legend('bottom',
       legend = c("Datos", "NLS", "Momentos"),
       lty = c(0,2,1), pch = c(1,NA,NA),
       col = c("black","red","blue"))
title('Ajuste de Datos a distribucion Normal', cex = 0.8)


#___________________________________________________________
NS = Nelson.Siegel(fp,xp)
NSCurve = function(x){
  b0 = NS[1]
  b1 = N2[2]
  b2 = N3[3]
  l = NS[4]
  return(b0+b1*(1-exp(-l*x)) / (l*x)+b2*((1-exp(-l*x))(l*x)-exp(-l*x)))
}
curve(NSCurve,0,30, add=T, col= 'blue')


#----------------------------------------------------
Plazo = c(1/12, 2/12, 3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30)
colores = rainbow(nrow(USTBonds) -1)
plot(Plazo, USTBonds[1,], ylim = c(1, 2.5), type = "b", lty = "dotted",
     pch = 16, ylab = "rate")
for (i in 2:nrow(USTBonds)) {
  points(Plazo, USTBonds[i,], col = colores[i-1], type = "b", lty = "dotted", pch = 16)
}; rm(i)
legend("bottomright", legend = row.names(USTBonds),
       col = c("black", colores), cex = 0.75, pch = 16)

# Grafico para un dia en particular
k = 2 #Dia 2
xp = Plazo
fp = as.double(USTBonds[k,])
plot(xp, fp, ylim = c(1, 2.5), type = "b", lty="dotted", pch = 16,
     xlab = "Plazo", ylab = paste("Yield curve on", row.names(USTBonds)[k]))


#-------------------------------------------------------------------------
# Ajuste del modelo de Nelson-Siegel ----
# install.packages("YieldCurve")
library("YieldCurve")
NS = Nelson.Siegel(fp, xp)
NScurve = function(x){ #1.02
  b0 = NS[1]
  b1 = NS[2]
  b2 = NS[3]
  l = NS[4]
  return(b0 + b1*(1-exp(-l*x))/(l*x) + b2*((1 - exp(-l*x))/(l*x) - exp(-l*x)))
}
curve(NScurve, 0, 30, add = T, col = "blue")

