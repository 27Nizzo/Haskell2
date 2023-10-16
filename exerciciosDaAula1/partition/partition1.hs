partition2 :: Ord a => a -> [a] -> ([a],[a]) 
partition2 x [] = ([x],[])
partition2 x(h:t) = if h <= x then (h:xs,ys)
                        else (xs,h:ys)
    where (xs,ys) = partition2 x t
    