


-- Exercicio: DEfina a função para calcular o comprimento de uma linha poligonal

type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double

dist :: Ponto -> Ponto -> Double
dist (Cartesiano a b) (Cartesiano as bs) = sqrt((as - a)^2 + (bs - b)^2)

comprimento :: Poligonal -> Double
comprimento [x] = 0
comprimento (x:y:t) = dist x y + comprimento(y:t)