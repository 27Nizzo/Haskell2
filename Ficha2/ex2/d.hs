soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h >= 0 then h : soPos t 
                else soPos t    

-- verificamos primeiramente se a head é positiva ou negativa, se for positiva entao checkmos de seguida o resto da lista(tail)
-- se for negativa checkamos só a tail.
