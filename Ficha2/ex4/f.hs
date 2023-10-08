type Polinomio = [Monomio]
type Monomio = (Float,Int)

simp :: Polinomio -> Polinomio
simp [] = []
simp ((a,b):t) | a == 0 = simp t
               | otherwise = (a,b) : simp t