data Ponto = Cartesiano Double Double | Polar Double Double deriving(Show,Eq)

posy :: Ponto -> Double 
posy Cartesiano y = y
posy Polar x = x * sin(y)
