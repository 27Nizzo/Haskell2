type Hora = (Int, Int)

converter2 :: Int -> Hora
converter2 x = (div x 60, mod x 60)

converter :: Hora -> Int
converter (h,m) = h * 60 + m

addMin :: Int -> Hora -> Hora
addMin x hora = converter2(x + converter hora)