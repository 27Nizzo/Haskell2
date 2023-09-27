data Ponto = Cartesiano Double Double | Polar Double Double deriving(Show,Eq)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt(x^2 + y^2)
raio (Polar raio _) = raio
-- A distancia do ponto a origem utilizando o raio é o próprio raio