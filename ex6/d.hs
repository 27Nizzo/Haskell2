data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan2 y x
angulo (Polar _ theta) = theta
