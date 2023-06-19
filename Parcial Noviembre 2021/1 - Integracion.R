# 1 - INTEGRACION ----

## Regla del trapecio: n = 1   
Integral_ReglaTrapecio<-function(x,f){
  h=x[2]-x[1]
  return((h/2)*(f(x[1])+f(x[2])))
}

## Regla de Simpson: n = 2 
Integral_ReglaSimpson<-function(x,f){
  h=(x[2]-x[1])/2
  return((h/3)*(f(x[1])+4*f(x[1]+h)+f(x[1]+2*h)))
}

## Regla de 3 Octavos de Simpson: n = 3 
Integral_ReglaSimpson_TresOctavos<-function(x,f){
  h=(max(x)-min(x))/3
  return(((3*h)/8)*(f(x[1])+3*f(x[1]+h)+3*f(x[1]+2*h)+f(x[1]+3*h)))
}

# Regla Compuesta Simpson 
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

## Regla Compuesta del Trapecio 
ReglaCompuestaTrapezoidal<-function(a,b,n,f){
  h=(b-a)/n
  I1=0
  for(i in 1:(n-1)){
    I1=I1+f(a+i*h)
  }
  return(h/2*(f(a)+2*I1+f(b)))
}

# 1.1 ----
alpha = 4.36
theeta = 2.52
x = c(1.15, 4.99)
f = function(x){(alpha^2*(x/theeta)^alpha)/(x*(1+(x/theeta)^alpha)^(alpha+1))}

Integral_ReglaTrapecio(x, f) #0.8734124, nodos y0, y1 
Integral_ReglaSimpson(x, f) #0.3472817 nodos y0, y1, y2
Integral_ReglaSimpson_TresOctavos(x, f) #0.5722941 nodos y0, y1, y2, y3

# 1.2 ----
ReglaCompuestaTrapezoidal(1.15, 4.99, 17, f) # 0.8646727 y0 a y17

# Cota de error
funcion = expression((alpha^2*(x/theeta)^alpha)/(x*(1+(x/theeta)^alpha)^(alpha+1)))

x  =  seq(1.15, 4.99, length.out = 10000)
fx = eval(D(D(funcion, "x"),"x"), list(x = x))

limitesuperior = 4.99
limiteinferior = 1.15
n = 17
h = (limitesuperior - limiteinferior)/n

# Trapecio compuesta
((limitesuperior - limiteinferior)/12)*(h^2)* max(abs(fx))

#0.0606897

# Es mas precisa en compuesta por que se separa en intervalos, para reducir el impacto de la naturales oscilante de los polinomios. 

# 1.3 ----
f = function(x){x*((alpha^2*(x/theeta)^alpha)/(x*(1+(x/theeta)^alpha)^(alpha+1)))}
ReglaCompuestaTrapezoidal(0.00001, 100, 474, f)

Esperanza = ReglaCompuestaTrapezoidal(0.00001, 100, 474, f)

# Cota de error
funcion = expression(x*((alpha^2*(x/theeta)^alpha)/(x*(1+(x/theeta)^alpha)^(alpha+1))))

x  =  seq(0.00001, 100, length.out = 10000)
fx = eval(D(D(funcion, "x"),"x"), list(x = x))

limitesuperior = 100
limiteinferior = 0.00001
n = 474
h = (limitesuperior - limiteinferior)/n

# Trapecio compuesta
((limitesuperior - limiteinferior)/12)*(h^2)* max(abs(fx))
#2.559786




# 1.4 ----
EsperanzaSC = ReglaCompuestaSimpson(0.00001, 100, 474, f)
f2 = function(x){x^2*((alpha^2*(x/theeta)^alpha)/(x*(1+(x/theeta)^alpha)^(alpha+1)))}
E2 = ReglaCompuestaSimpson(0.00001, 100, 474, f2)
Var = E2^2 - EsperanzaSC
# 7.97384175796215

# Cota de error: No hay derivada cuarta, no es posible hallar este valor


