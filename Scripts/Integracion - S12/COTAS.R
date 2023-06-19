############ COTAS DE ERROR #######################
library(Deriv)

a = 1
b = 2

alpha = 4.36
theta = 2.25
f = function(x){
  (alpha^2 * (x/theta)^alpha)/(x*(1 + (x/theta)^alpha)^(alpha+1))
}

## DERIVADA. 3 PUNTOS
### ENDPOINT
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2, "x")
ddfx = fprima3(x)

h = 10^-10 #uso un numero bien chico como paso

error_3_endpoint = h^2 /3 * max(abs(ddfx))

print(paste("La cota de error es ", error_3_endpoint))

### MIDPOINT
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2, "x")
ddfx = fprima3(x)

h = 10^-10 #uso un numero bien chico como paso

error_3_midpoint = -h^2 /6 * max(abs(ddfx))

print(paste("La cota de error es ", error_3_midpoint))

## DERIVADA. 5 PUNTOS
### ENDPOINT
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2, "x")
fprima4 = Deriv(fprima3, "x")
fprima5 = Deriv(fprima4,"x")
ddfx = fprima5(x)

h = 10^-10 #uso un numero bien chico como paso

error_5_endpoint = h^4 /(30) * max(abs(ddfx))

print(paste("La cota de error es ", error_5_endpoint))

### MIDPOINT
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2, "x")
fprima4 = Deriv(fprima3, "x")
fprima5 = Deriv(fprima4,"x")
ddfx = fprima5(x)

h = 10^-4 #uso un numero bien chico como paso

error_5_endpoint = h^4 /5 * max(abs(ddfx))

print(paste("La cota de error es ", error_5_endpoint))


## DERIVADA SEGUNDA
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2, "x")
fprima4 = Deriv(fprima3, "x")
ddfx = fprima4(x)

h = 10^-4 #uso un numero bien chico como paso

error_segunda = h^2 /12 * max(abs(ddfx))

print(paste("La cota de error es ", error_segunda))



## REGLA DEL TRAPECIO
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
ddfx = fprima2(x)

h = 10^-4 #uso un numero bien chico como paso

error_trapecio = -h^3 /12 * max(abs(ddfx))

print(paste("La cota de error es ", error_trapecio))


## REGLA DE SIMPSON
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2,"x")
fprima4 = Deriv(fprima3,"x")
ddfx = fprima4(x)

h = 10^-4 #uso un numero bien chico como paso

error_simpson = -h^5 /90 * max(abs(ddfx))

print(paste("La cota de error es ", error_simpson))

## REGLA DE SIMPSON 3/8
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2,"x")
fprima4 = Deriv(fprima3,"x")
ddfx = fprima4(x)

h = 10^-4 #uso un numero bien chico como paso

error_simpson38 = -(3*h^5) /80 * max(abs(ddfx))

print(paste("La cota de error es ", error_simpson38))

## n = 4
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2,"x")
fprima4 = Deriv(fprima3,"x")
fprima5 = Deriv(fprima4,"x")
fprima6 = Deriv(fprima5,"x")
ddfx = fprima6(x)

h = 10^-4 #uso un numero bien chico como paso

error_n4_cerrado = -(8*h^7) /945 * max(abs(ddfx))

print(paste("La cota de error es ", error_n4_cerrado))


## NC ABIERTAS
### n = 0
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
ddfx = fprima2(x)

h = 10^-4 #uso un numero bien chico como paso

error_n0_abierto = h^3/3 * max(abs(ddfx))

print(paste("La cota de error es ", error_n0_abierto))

### n = 1
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
ddfx = fprima2(x)

h = 10^-4 #uso un numero bien chico como paso

error_n1_abierto = (3*h^3)/4 * max(abs(ddfx))

print(paste("La cota de error es ", error_n1_abierto))

### n = 2
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2,"x")
fprima4 = Deriv(fprima3,"x")
ddfx = fprima4(x)

h = 10^-4 #uso un numero bien chico como paso

error_n2_abierto = (14*h^5)/45 * max(abs(ddfx))

print(paste("La cota de error es ", error_n2_abierto))

### n = 3
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2,"x")
fprima4 = Deriv(fprima3,"x")
ddfx = fprima4(x)

h = 10^-4 #uso un numero bien chico como paso

error_n3_abierto = ((95/144)*h^5) * max(abs(ddfx))

print(paste("La cota de error es ", error_n3_abierto))

## REGLA TRAPECIO COMPUESTO
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
ddfx = fprima2(x)

h = (a - b)/n

error_trapecio_compuesto = ((a - b)/12) * (h^2) * max(abs(ddfx))

## REGLA SIMPSON COMPUESTO
x = seq(a, b, length.out = 10000)

fprima = Deriv(f,"x")
fprima2 = Deriv(fprima,"x")
fprima3 = Deriv(fprima2,"x")
fprima4 = Deriv(fprima3, "x")
ddfx = fprima4(x)

h = (a - b)/n

error_simpson_compuesto = -((b-a)/180) *h^4 * max(abs(ddfx))
