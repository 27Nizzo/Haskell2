type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- calcula o resultado da multiplicação de de um monómio por um polinómio
-- EX : (2,2) * [(1,3),(3,2),(4,1)] = [(2,0,5),(6,0,4),(8,0,3)]

mult :: Monomio -> Polinomio -> Polinomio 
mult _ [] = [] 
mult (a,b) ((a1,b1):t) = (a * a1, b + b1) : mult (a,b) t