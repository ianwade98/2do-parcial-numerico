# NODOS DE INTEGRACION
 # TRAPECIO SIMPLE
#NODOS:
#x0 = a = 3.38
#x1= b= 3.99

#SIMPSON
#NODOS:
#h = (b-a)/n = (3.99-3.38)/2
#xo = a = 3.38
#x1= x0 + h = 3.38 + (3.99-3.38)/2
#x2 = b = 3.99

# TRES OCTAVOS
#NODOS:
#h = (b-a)/n = (3.99-3.38)/3
#xo = a = 3.38
#x1= x0 + h = 3.38 + (3.99-3.38)/3
#x2 = x0 + 2*h = 3.38 + 2*(3.99-3.38)/3
#x3 = b = 3.99

# COMPUESTA SIMPSON
#NODOS: 
# a = 3.38
# b = 3.99
# h = (b - a)/n = (3.38 - 3.99)/28
# x = a + i*h for i in 0:n


#Se puede notar que las aproximaciones son muy similares entre las del 1.1 y la del 1.2.
#La que más se aproxima a compuesta simpson es la de tres octavos simpson cerrada.
#Esto se puede dar ya que el metodo de tres octavos es mas preciso que el simple de simpson y trapecio.

