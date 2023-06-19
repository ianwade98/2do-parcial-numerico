#Facundo Matías De Lorenzo

#Registro: 895187

#EJERCICIO 1: INTEGRACIÓN-----

alpha=1.99
tita=1.32

f<- function(x) { ((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}

#calculo gamma

gammaalpha = gamma(alpha)

#1.1 Probabilidades simples------
a=3.38
b=3.99


Trapecio_Cerrada <- function(a,b){
  n = 1
  h = (b-a)/n
  x0= a
  x1=b
  Resultado = (h/2)*(f(x0)+f(x1))
  return(Resultado)
}

Simpson_Cerrada <- function(a,b){
  n = 2
  h = (b-a)/n
  x0 = a
  x1 = x0 + h
  x2= b
  Resultado = (h/3)*(f(x0)+4*f(x1)+f(x2))
  return(Resultado)
}

Tres_octavos_simpson_Cerrada <- function(a,b){
  n=3 
  h = (b-a)/n
  x0 = a
  x1 = x0 + h
  x2= x0 + 2*h 
  x3 = b
  Resultado = ((3*h)/8)*(f(x0)+3*f(x1)+3*f(x2)+f(x3))
  return(Resultado)
}

Trapecio_Cerrada(a,b) #0.01554264

#NODOS:
#x0 = a = 3.38
#x1= b= 3.99

Simpson_Cerrada(a,b) #0.0152221

#NODOS:
#h = (b-a)/n = (3.99-3.38)/2
#xo = a = 3.38
#x1= x0 + h = 3.38 + (3.99-3.38)/2
#x2 = b = 3.99


Tres_octavos_simpson_Cerrada(a,b) #0.01522164

#NODOS:
#h = (b-a)/n = (3.99-3.38)/3
#xo = a = 3.38
#x1= x0 + h = 3.38 + (3.99-3.38)/3
#x2 = x0 + 2*h = 3.38 + 2*(3.99-3.38)/3
#x3 = b = 3.99


#1.2 probabilidades compuesto-----

#como n es impar, utilizo n=28 para poder usar estimación de compuesta simpson
a = 3.38
b= 3.99
n=28

Compuesta.Simpson <- function(a,b,n){
  h = (b-a)/n
  xi0 = f(a) + f(b)
  xi1 = 0
  xi2 = 0
  for(i in 1:(n-1)){
    x = a + i*h
    if(i%%2==0){
      xi2= xi2 + f(x)
    } else {
      xi1 = xi1 + f(x)
    }
  }
  xi = h*(xi0+2*xi2+4*xi1)/3
  return(xi)
}

Compuesta.Simpson(a,b,n) #0.01522127

#NODOS: 
# a = 3.38
# b = 3.99
# h = (b - a)/n = (3.38 - 3.99)/28
# x = a + i*h for i in 0:n


#Se puede notar que las aproximaciones son muy similares entre las del 1.1 y la del 1.2.
#La que más se aproxima a compuesta simpson es la de tres octavos simpson cerrada.
#Esto se puede dar ya que el metodo de tres octavos es mas preciso que el simple de simpson y trapecio.


#COTA DE ERROR
FD <- expression(  ((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha))

Derivada = D(D(D(D(FD,"x"),"x"),"x"),"x")

Derivada2_func = function(x){eval(Derivada)}

Derivada2_func(1)

curve(Derivada2_func,3.38,3.99)
a=3.38
b=3.99
n=28
h=(b-a)/n
x= seq(a,b,length.out=1000)
y = Derivada2_func(x)
points(x,y)
M=max(abs(y))
cota = (b-a)/180 * (h^4) * M 



#1.3 esperanza ------

f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}

n=354

curve(f,10^-10,300)

Compuesta.Simpson(10^-10,300,n) #1.378505


#COTA DE ERROR
FD <- expression( x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha ) )

Derivada = D(D(D(D(FD,"x"),"x"),"x"),"x")

Derivada2_func = function(x){eval(Derivada)}

Derivada2_func(1)

curve(Derivada2_func,10^-10,300)
a=10^-10
b=300
n=354
h=(b-a)/n
x= seq(a,b,length.out=1000)
y = Derivada2_func(x)
points(x,y)
M=max(abs(y))
cota = (b-a)/180 * (h^4) * M 



#1.4 derivada de e(y) ------

#respecto de alpha
f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}

n=354

fE1 =  Compuesta.Simpson(10^-10,300,n)

h = 10^-10
alpha= alpha + h

f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}

gammaalpha = gamma(alpha)
fE2 = Compuesta.Simpson(10^-10,300,n)

Derivada1 = (fE2 - fE1)/h 


#respecto de tita
f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}

n=354
alpha=1.99
tita=1.32
gammaalpha = gamma(alpha)
fE1 =  Compuesta.Simpson(10^-10,300,n)


h = 10^-10
tita= tita + h

f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}

gammaalpha = gamma(alpha)
fE2 = Compuesta.Simpson(10^-10,300,n)

Derivada2 = (fE2 - fE1)/h 



#1.5 VARIANZA -----
n=354
alpha=1.99
tita=1.32
gammaalpha = gamma(alpha)
f <- function(x){  x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}

Compuesta_Trapecio = function(a, b, n){
  h = (b-a)/n
  xi0 = f(a) + f(b)
  xi = 0
  for (i in 1:(n-1)) {
    x = a + i*h
    xi = xi + f(x)
  }
  Resultado = h*(xi0 + 2*xi)/2
  return(Resultado)
}

curve(f,10^-10,300)
A = Compuesta_Trapecio(10^-10,300,354) #1.285094


f <- function(x){ (x-A)^2*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
curve(f,10^-10,300)
Compuesta_Trapecio(10^-10,300,354) #6.730076


#COTA DE ERROR


FD <- expression(  (x-A)^2*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha) )

Derivada = D(D(FD,"x"),"x")

Derivada2_func = function(x){eval(Derivada)}

Derivada2_func(2)

curve(Derivada2_func,10^-10,300)
a=10^-10
b=300
n=354
h=(b-a)/n
x= seq(a,b,length.out=1000)
y = Derivada2_func(x)
points(x,y)
M=max(abs(y))
cota = (b-a)/12 * (h^2) * M


#1.6 derivacion varianza-----

#respecto de alpha 
n=354
alpha=1.99
tita=1.32


f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
A = Compuesta_Trapecio(10^-10,300,354) 
f <- function(x){ (x-A)^2*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
V1 = Compuesta_Trapecio(10^-10,300,354) 

h = 10^-10
alpha = alpha + h

gamma(alpha)

f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
A2 = Compuesta_Trapecio(10^-10,300,354) 

f <- function(x){ (x-A2)^2*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
V2 = Compuesta_Trapecio(10^-10,300,354) 

DERIVADA1 = (V2-V1)/h

#respecto de tita
n=354
alpha=1.99
tita=1.32
gamma(alpha)

f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
A = Compuesta_Trapecio(10^-10,300,354) 
f <- function(x){ (x-A)^2*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
V1 = Compuesta_Trapecio(10^-10,300,354) 

h = 10^-10
tita = tita + h

gamma(alpha)

f<- function(x) { x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
A2 = Compuesta_Trapecio(10^-10,300,354) 
f <- function(x){ (x-A2)^2*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gammaalpha)}
V2 = Compuesta_Trapecio(10^-10,300,354) 

DERIVADA2 = (V2-V1)/h



