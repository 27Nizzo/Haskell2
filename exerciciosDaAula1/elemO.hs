elemO :: Ord a => a -> [a] -> Bool
elemO x [] = False
elemO x (h:t)  | x == h = True
               | x < h = False
               | otherwise = elemO x t
