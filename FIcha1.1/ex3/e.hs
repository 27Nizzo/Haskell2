horas :: Int -> Int
horas x = x * 60 

difHor :: (Int, Int) -> (Int, Int) -> Int
difHor (x1,y1) (x2,y2) = abs((horas(x1) + y1) - (horas(x2) + y2))