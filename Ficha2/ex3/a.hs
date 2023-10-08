

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if('1' <= h && h <= '9') 
                then h : soDigitos t
                else soDigitos t
