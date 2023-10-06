merge1 :: Ord a => [a] -> [a] -> [a]

--merge1 [1,2,5] [3,4,7] = [1,2,3,4,5,7]

merge1 [] l2 = l2
merge1 l1 [] = l1
merge1 (x:xs) (y:ys) | x <= y = x : merge1 xs (y:ys)
                     | otherwise = y : merge1 (x:xs) ys

    --if x <=y then x : merge1 xs (y:ys)
                               -- else y : merge1 (x:xs) ys -> este caso só resulta para numeros 