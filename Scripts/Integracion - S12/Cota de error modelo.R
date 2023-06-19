# Aca cambiar mi funcion, function por expression y ponerle funcion de nombre
funcion = expression((4.36^2*(x/2.52)^4.36)/(x*(1+(x/2.52)^4.36)^(4.36 +1)))

x  =  seq(1.15, 4.99, length.out = 10000)
fx = eval(D(D(funcion, "x"),"x"), list(x = x))

limitesuperior = 4.99
limiteinferior = 1.15
n = 17
h = (limitesuperior - limiteinferior)/n

# Trapecio compuesta
((limitesuperior - limiteinferior)/12)*(h^2)* max(abs(fx))

# Simpson compuesta
fx = eval(D(D(D(D(funcion, "x"),"x"), list(x = x))))
((limitesuperior - limiteinferior)/180)*(h^4)* max(abs(fx))

