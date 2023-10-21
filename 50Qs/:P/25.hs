elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 _ [] = []
elemIndices1 e l = aux e l 0

aux :: Eq a => a -> [a] -> Int -> [Int]
aux _ [] _ = []
aux e (h:t) n = if e == h then n : aux e t (n+1) else aux e t (n+1)
