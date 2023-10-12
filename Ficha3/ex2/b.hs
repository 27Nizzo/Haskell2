

-- Exercicio: FUnção que testa se alinha poligonal é ou não fechada

type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double

fechada :: Poligonal -> Bool
fechada [p1,p2] = False
fechada [p1] = False 
fechada (p1:p2:p3:ps) = if p1 == last(ps) then True else False 

-- NOTA: para ser fechado o primeiro e o ultimo ponto tem que ter no minimo x ou o y em comum