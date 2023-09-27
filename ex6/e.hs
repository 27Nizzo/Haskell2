 data Ponto = Cartesiano Double Double | Polar Double Double   deriving (Show,Eq)
 
 dist :: Ponto -> Ponto -> Double 
 dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)
 
 dist (Cartesiano x1 y1) (Polar r2 theta2) = 
    let x2 = r2 * cos theta2
        y2 = r2 * sin theta2
        in sqrt((x2 - x1)^2 + (y2 - y1)^2)
 
 dist (Polar r1 theta1) (Cartesiano x2 y2) =
    let x1 = r1 * cos theta1
        y1 = r1 * sin theta1
        in sqrt((x2 - x1)^2 + (y2 - y1)^2)