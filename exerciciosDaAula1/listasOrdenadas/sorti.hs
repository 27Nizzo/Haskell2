
insert1 :: Ord a => a -> [a] -> [a]
insert1 x [] = [x]
ĩnsert1 x (h:t) | x < h = x : h : t
                | otherwise h : insert1 x t


sorti :: Ord a => [a] -> [a]
sorti [] = []
sorti [x] = [x]
sorti (h:t) = insert1 h (sorti t)
 