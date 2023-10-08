minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if (h >= 'a' && h <= 'z')    
                    then minusculas t + 1
                    else minusculas t  