data Ponto = Cartesiano Double Double
            | Polar Double Double
            deriving(Show, Eq)

data Figura = Circulo Ponto Double
                | Rectangulo Ponto Ponto
                |  Triangulo Ponto Ponto Ponto
                deriving (Show, Eq)

poligno :: Figura -> Bool 
poligno f = case f of
        Circulo _ _-> False
        _ -> True