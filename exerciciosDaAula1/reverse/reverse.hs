-- uma função onde reverte uma lista: [1,2,3,4] -> [4,3,2,1]

reverse1 :: [a] -> [a] -> [a]
reverse1 [] [] = []
reverse1 (h, t) = (reverse t) ++ [h]