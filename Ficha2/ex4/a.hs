type Polinomio = [Monomio]
type Monomio = (Float,Int)
--Ex: [(2,3), (3,4), (5,3), (4,5)] = 2x^3 + 3x^4 + 5x^3 +4x^5 

-- indica quantos monomios de grau n existem em p

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((a,b):t) = if b == n then 1 + conta n t
                            else conta n t