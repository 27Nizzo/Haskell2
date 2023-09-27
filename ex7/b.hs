data Figura = Circulo Ponto Double
| Rectangulo Ponto Ponto
| Triangulo Ponto Ponto Ponto
deriving (Show,Eq)

data Ponto = Cartesiano Double Double | Polar Double Double
deriving (Show,Eq)

vertices :: Figura -> [Ponto] 
vertices (Rectangulo ponto1 ponto2) =
    [ponto1, Cartesiano (posx ponto1) (posy ponto2), ponto2, Cartesiano (posx ponto2) (posy ponto1)]
vertices (Circulo centro raio) = 
    [Cartesiano (posx centro + raio) (posy centro), 
    Cartesiano (posx centro - raio) (posy centro)] 