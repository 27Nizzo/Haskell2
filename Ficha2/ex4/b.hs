type Polinomio = [Monomio]
type Monomio = (Float,Int)

grau :: Polinomio -> Int
grau [] = 0
grau ((a,b):(a1,b1):t) = if b > b1 then b 
                            else b1