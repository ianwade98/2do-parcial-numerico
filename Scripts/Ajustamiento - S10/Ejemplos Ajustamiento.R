rm(list=ls())
graphics.off()

###MINIMOS CUADRADOS

#Ejemplo1
xp<-c(1,1.3,1.6,1.9,2.2)
fp<-c(0.7651977,0.6200860,0.4554022,0.2818186,0.1103623)
x1<-xp
x2<-xp^2
x3<-xp^3
x4<-xp^4
y=lm(fp~x1+x2+x3+x4)
y
y$coefficients
y$residuals

#Ejemplo2
xp<-c(1,1.3,1.6,1.9,2.2)
fp<-c(0.7651977,0.6200860,0.4554022,0.2818186,0.1103623)
x1<-xp
x2<-xp^2
x3<-xp^3
y2=lm(fp~x1+x2+x3)
y2
y2$coefficients
y2$residuals


#Ejemplo3
xp<-c(1,1.3,1.6,1.9,2.2)
fp<-c(0.7651977,0.6200860,0.4554022,0.2818186,0.1103623)
x1<-xp
x2<-xp^2
y3=lm(fp~x1+x2)
y3
y3$coefficients
y3$residuals


#Ejemplo4
xp<-c(1,1.3,1.6,1.9,2.2)
fp<-c(0.7651977,0.6200860,0.4554022,0.2818186,0.1103623)
x1<-xp
y4=lm(fp~x1)
y4
y4$coefficients
y4$residuals


######### Ejercicio
#grado4
xp<-c(3.05,3.86,4.67,5.48,6.29,7.1,7.91)
fp<-c(0.3619,-3.048,-10.0123,-3.4302,0.0272,2.7344,3.6877)
x1<-xp
x2<-xp^2
x3<-xp^3
x4<-xp^4
x5<-xp^5
x6<-xp^6
x7<-xp^7
y=lm(fp~x1+x2+x3+x4+x5+x6+x7)
y
y$coefficients
y$residuals

#grado7
xp<-c(3.05,3.86,4.67,5.48,6.29,7.1,7.91)
fp<-c(0.3619,-3.048,-10.0123,-3.4302,0.0272,2.7344,3.6877)
x1<-xp
x2<-xp^2
x3<-xp^3
x4<-xp^4
x5<-xp^5
x6<-xp^6
x7<-xp^7
y7=lm(fp~x1+x2+x3+x4+x5+x6)
y7
y7$coefficients
y7$residuals


#grado6
xp<-c(3.05,3.86,4.67,5.48,6.29,7.1,7.91)
fp<-c(0.3619,-3.048,-10.0123,-3.4302,0.0272,2.7344,3.6877)
x1<-xp
x2<-xp^2
x3<-xp^3
x4<-xp^4
x5<-xp^5
x6<-xp^6
y6=lm(fp~x1+x2+x3+x4+x5+x6)
y6
y6$coefficients
y6$residuals


#grado5
xp<-c(3.05,3.86,4.67,5.48,6.29,7.1,7.91)
fp<-c(0.3619,-3.048,-10.0123,-3.4302,0.0272,2.7344,3.6877)
x1<-xp
x2<-xp^2
x3<-xp^3
x4<-xp^4
x5<-xp^5
y5=lm(fp~x1+x2+x3+x4+x5)
y5
y5$coefficients
y5$residuals


#grado4
xp<-c(3.05,3.86,4.67,5.48,6.29,7.1,7.91)
fp<-c(0.3619,-3.048,-10.0123,-3.4302,0.0272,2.7344,3.6877)
x1<-xp
x2<-xp^2
x3<-xp^3
x4<-xp^4
y4=lm(fp~x1+x2+x3+x4)
y4
y4$coefficients
y4$residuals

#grado3
xp<-c(3.05,3.86,4.67,5.48,6.29,7.1,7.91)
fp<-c(0.3619,-3.048,-10.0123,-3.4302,0.0272,2.7344,3.6877)
x1<-xp
x2<-xp^2
x3<-xp^3
y3=lm(fp~x1+x2+x3)
y3
y3$coefficients
y3$residuals

#grado2
xp<-c(3.05,3.86,4.67,5.48,6.29,7.1,7.91)
fp<-c(0.3619,-3.048,-10.0123,-3.4302,0.0272,2.7344,3.6877)
x1<-xp
x2<-xp^2
y2=lm(fp~x1+x2)
y2
y2$coefficients
y2$residuals



#grado1
xp<-c(3.05,3.86,4.67,5.48,6.29,7.1,7.91)
fp<-c(0.3619,-3.048,-10.0123,-3.4302,0.0272,2.7344,3.6877)
x1<-xp
y1=lm(fp~x1)
y1
y1$coefficients
y1$residuals

fp
fitted(y1) #grado1
fitted(y2) #grado2
fitted(y3) #grado3
fitted(y4) #grado4
fitted(y5) #grado5
fitted(y6) #grado6
fitted(y7) #grado7

plot(xp,fp)
points(xp,fitted(y1),col='red')
points(xp,fitted(y2),col='blue')
points(xp,fitted(y3),col='green')
points(xp,fitted(y4),col='yellow')
points(xp,fitted(y5),col='pink')
points(xp,fitted(y6),col='orange')
points(xp,fitted(y7),col='purple')