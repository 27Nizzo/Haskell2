-- REcebe uma lista e reverte a sua ordem de elementos

reverse2 :: [a] -> [a] 
reverse2 [] = []
reverse2 l = (last l) : reverse(init l)
