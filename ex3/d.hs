--d) converter um valor em minutos para horas;


conv :: Int -> Int 
conv y1 = y1 `div` 60

convhor :: (Int, Int) -> Int
convhor (x1,y1) = conv(y1) + x1 