data Ponto = Cartesiano Double Double | Polar Double Double deriving(Show,Eq)

posx :: Ponto -> Double
posx Cartesiano x = x
posx Polar x = x * cos(y)

