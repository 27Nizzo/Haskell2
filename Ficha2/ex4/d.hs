type Polinomio = [Monomio]
type Monomio = (Float,Int)

deriv :: Polinomio -> Polinomio 
deriv [] = []
deriv ((a,0):t) = deriv t
deriv ((a,b):t) = (a * fromIntegral b, b - 1) : deriv t

--derivada de um ploninomio: b * ax^b-1 ex: (2,2) || 2x^2 = (4,1) = (b*a,b-1)
