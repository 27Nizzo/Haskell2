elem2 :: Eq a => a -> [a] -> Bool
elem2 x [] = False
elem2 x (h:t) | h == x = True
              | otherwise = elem2 x t
