-- A fun¸c˜ao nRaizes que recebe os (3) coeficientes de um polin´omio de 2o grau e que calcula o n´umero de ra´ızes (reais) desse polin´omio.
nRaizes :: Int -> Int -> Int -> Int
nRaizes a b c = if d > 0 
                then 2
                else if d == 0
                     then 1
                     else 0
    where d = b^2 - 4ac