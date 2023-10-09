type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- dado um polinomio constrói um polinomio equivalente em que nao podem aparecer vários monómios do mesmo grau
normaliza :: Polinomio -> Polinomio 
{-normaliza [] = []
normaliza [(a,b)] = [(a,b)]
normaliza ((a,b):(a1,b1):t) | b == b1 = normaliza ((a + a1,b):t)
                            | otherwise = (a,b) : normaliza ((a1,b1):t)
-}

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((a,b):t) = if b == n then 1 + conta n t
                            else conta n t

normaliza [] = []
normaliza [(a,b)] = [(a,b)]
normaliza ((a,b):(a1,b1):t) | b == b1 = normaliza ((a+a1,b):t)
                            | conta b t == 0 = (a,b) : normaliza ((a1,b1):t)
                            | otherwise = normaliza((a,b): t ++ [(a1,b1)])
--ex: normaliza [(1,2),(3,2)] = [(4,0,2)]
--ex2: normaliza [(2,2), (1,3), (1,2)] = [(3,0,2),(1,0,3)]