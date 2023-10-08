type Polinomio = [Monomio]
type Monomio = (Float,Int)


selgrau :: Int -> Polinomio -> Polinomio 
selgrau _ [] = []
selgrau n ((a,b):t) = if n == b then (a,b) : selgrau n t 
                                else selgrau n t