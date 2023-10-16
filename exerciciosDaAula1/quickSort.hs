quickSort1 :: Ord a => [a] -> [a]
quickSort1 [] = []
quickSort1 (h:t) = 
                let (a,b) = partition2 h t
                in quickSort1 a ++ [h] ++ quickSort1 b

partition2 :: Ord a => a -> [a] -> ([a],[a]) 
partition2 x [] = ([x],[])
partition2 x(h:t) = if h <= x then (h:xs,ys)
                        else (xs,h:ys)
    where (xs,ys) = partition2 x t