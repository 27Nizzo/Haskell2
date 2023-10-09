type Polinomio = [Monomio]
type Monomio = (Float,Int)

mult :: Monomio -> Polinomio -> Polinomio 
mult _ [] = [] 
mult (a,b) ((a1,b1):t) = (a * a1, b + b1) : mult (a,b) t

soma :: Polinomio -> Polinomio -> Polinomio 
soma [] l = l
soma l [] = l
soma l l1 = normaliza (l ++ l1)  

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((a,b):t) = if b == n then 1 + conta n t
                            else conta n t

normaliza [] = []
normaliza [(a,b)] = [(a,b)]
normaliza ((a,b):(a1,b1):t) | b == b1 = normaliza ((a+a1,b):t)
                            | conta b t == 0 = (a,b) : normaliza ((a1,b1):t)
                            | otherwise = normaliza((a,b): t ++ [(a1,b1)])

produto :: Polinomio -> Polinomio -> Polinomio
produto [(a,b)] [] = []
produto [] [(a,b)] = []
produto (h:t) (a1:b1) = soma (mult h (a1:b1)) (produto t (a1:b1))
