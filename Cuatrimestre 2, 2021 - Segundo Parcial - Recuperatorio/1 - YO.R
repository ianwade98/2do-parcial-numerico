# 1 - INTEGRACION
alpha = 1.99
tita = 1.32
x = c(3.38, 3.99)
f = function(x){((tita/x)^(alpha)*exp(-(tita/x)))/(x*gamma(alpha))}

# 1.1 ----
alpha = 1.99
tita = 1.32
x = c(3.38, 3.99)
f = function(x){((tita/x)^(alpha)*exp(-(tita/x)))/(x*gamma(alpha))}

## Regla del trapecio: n = 1  Nodos y0, y1
Integral_ReglaTrapecio<-function(x,f){
  h=x[2]-x[1]
  return((h/2)*(f(x[1])+f(x[2])))
}

## Regla de Simpson: n = 2 Nodos y0, y1, y2
Integral_ReglaSimpson<-function(x,f){
  h=(x[2]-x[1])/2
  return((h/3)*(f(x[1])+4*f(x[1]+h)+f(x[1]+2*h)))
}

## Regla de 3 Octavos de Simpson: n = 3 Nodos y0, y1, y2, y3
Integral_ReglaSimpson_TresOctavos<-function(x,f){
  h=(max(x)-min(x))/3
  return(((3*h)/8)*(f(x[1])+3*f(x[1]+h)+3*f(x[1]+2*h)+f(x[1]+3*h)))
}

Integral_ReglaTrapecio(x, f)
# 0.01554263513
Integral_ReglaSimpson(x, f)
# 0.01522209924
Integral_ReglaSimpson_TresOctavos(x, f)
# 0.0152216415

# 1.2 ----
ReglaCompuestaSimpson<-function(a,b,n,f){
  h=(b-a)/n
  I0=f(a)+f(b)
  I1=0
  I2=0
  x=0
  for(i in 1:(n-1)){
    x=a+i*h
    if(i%%2==0){
      I2=I2+f(x)
    }else{
      I1=I1+f(x)
    }
  }
  return(h*(I0+2*I2+4*I1)/3)
}

ReglaCompuestaSimpson(3.38, 3.99, 28, f)
# 0.01509408466
# Nodos y0, y1

# Cota de error
funcion = expression(((tita/x)^(alpha)*exp(-(tita/x)))/(x*gamma(alpha)))

x  =  seq(3.38, 3.99, length.out = 10000)
fx = eval(D(D(funcion, "x"),"x"), list(x = x))

limitesuperior = 3.38
limiteinferior = 3.99
n = 28
h = (limitesuperior - limiteinferior)/n

((limitesuperior - limiteinferior)/12)*(h^2)* max(abs(fx))

# -0.0000005811905367
# Los resultados obtenidos por Sipson Compuesto son mas precisos por que se reaizan por tramos



# 1.3 ----

EF = function(x){x*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gamma(alpha))}
ReglaCompuestaSimpson(3.38, 3.99, 354, EF)

# 1.4 ----

ReglaCompuestaTrapezoidal<-function(a,b,n,f){
  h=(b-a)/n
  I1=0
  for(i in 1:(n-1)){
    I1=I1+f(a+i*h)
  }
  return(h/2*(f(a)+2*I1+f(b)))
}

Esperanza1 = ReglaCompuestaTrapezoidal(3.38, 3.99, 354, EF)
E2 = function(x){x^2*((tita/x)^(alpha)*exp(-(tita/x)))/(x*gamma(alpha))}

Esperanza2 = ReglaCompuestaTrapezoidal(3.38, 3.99, 354, E2)

Esperanza2 - Esperanza1^2
Varianza = Esperanza2 - Esperanza1^2
