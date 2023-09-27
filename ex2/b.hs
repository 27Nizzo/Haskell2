--A fun¸c˜ao raizes que, usando a fun¸c˜ao anterior, recebe os coeficientes do polin´omio e calcula a lista das suas ra´ızes reais.
nRaizes :: Int -> Int -> Int -> Int
nRaizes a b c = if d > 0 
                then 2
                else if d == 0
                    then 1
                    else 0
                    where d = b^2 - 4*a*c


raizes :: Double -> Double -> Double -> [Double]
raizes a b c = if d > 0
                then [((-b) + sqrt(d))/(2*a) , ((-b) - sqrt(d))/(2*a)]
                else if d == 0 
                    then [(-b)/(2*a)]
                    else []

                    where d = b^2 - 4*a*c