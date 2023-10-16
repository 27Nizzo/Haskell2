-- Apresente uma função group que  agrupa elementos iguais e consecutivos de uma lista

groupp :: Eq a => [a] -> [[a]]
groupp [] = []
groupp (h:t) = takee h (h:t) : groupp (dropp h t)

takee :: Eq a => a -> [a] -> [a]
takee _ [] = []
takee x (h:t) | h == x = h : takee x t
              | otherwise = []

dropp :: Eq a => a -> [a] -> [a]
dropp _ [] = []
dropp x (h:t) | h == x = dropp x t
              | otherwise = h:t
