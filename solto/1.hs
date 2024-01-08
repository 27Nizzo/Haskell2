nome :: Int -> Int -> [Int]
-- nome 5 2 = [2,-2,2,-2,2]
nome n x = take n (cycle [x,-x])

replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n x = x : replicate'' (n-1) x

nome2 :: Int -> Int -> [Int]
nome2 0 _ = []
nome2 n x = x : nome2 (n-1) (-x)
 
