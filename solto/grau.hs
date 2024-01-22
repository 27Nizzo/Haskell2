    type Polinomio = [Monomio]
    type Monomio = (Float,Int)


    grau :: Polinomio -> Int 
    grau [] = 0 
    grau ((h,t):ts) = max t (grau ts)
    -- maximum de todos os t por toda lista
    