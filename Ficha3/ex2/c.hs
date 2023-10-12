type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double
                deriving(Show, Eq)

data Figura = Circulo Ponto Double
                | Rectangulo Ponto Ponto 
                | Triangulo Ponto Ponto Ponto
                deriving (Show , Eq)

--Exercicio: Calcula uma lista de triangulos cuja a soma das suas áreas seja igual à área delimitada pela linha poligonal

triangula :: Poligonal -> [Figura]
triangula [] = []
triangula [p1,p2,p3] = [(Triangulo p1 p2 p3)]
triangula (p1:p2:p3:ps) = (Triangulo p1 p2 p3) : triangula (p1:p3:ps)
