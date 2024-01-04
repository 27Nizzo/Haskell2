-- preCrescente [3,4,7,9,9,1] = [3,4,7,9]

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:t:ts) 
    | h == t = preCrescente (t:ts)  -- Elimina os numeros repetidos 
    | h < t = h : preCrescente (t:ts) -- se o numero for menor que o proximo, adiciona a lista
    | otherwise = [h]  -- se um elemento for maior que o seu seguinte caga nele e fecha a lista


amplitude :: [Int] -> Int 
amplitude [] = 0 
amplitude l = maximum l - minimum l 
