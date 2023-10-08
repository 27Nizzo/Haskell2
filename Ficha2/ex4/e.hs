type Polinomio = [Monomio]
type Monomio = (Float,Int)

calcula :: Float -> Polinomio -> Float 
calcula _ [] = 0.0
calcula x ((a,b):t) = a * (x^b) + calcula x t

