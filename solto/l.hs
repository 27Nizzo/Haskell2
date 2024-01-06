    intersperse1 :: a -> [a] -> [a]
    intersperse1 _ [] = []
    intersperse1 _ [x] = [x]
    intersperse1 n (h:t) = h : n : intersperse1 n t

    groupp :: Eq a => [a] -> [[a]]
    groupp [] = []
    groupp [x] = [[x]]
    groupp (h:t) = 
        takee h (h:t) : groupp (dropp h t)

    takee :: Eq a => a -> [a] -> [a]
    takee _ [] = []
    takee x (h:t) 
        | x == h = h : takee x t 
        | otherwise = []
    
    dropp :: Eq a => a -> [a] -> [a]
    dropp _ [] = []
    dropp x (h:t) 
        | x == h = dropp x t 
        | otherwise = h:t 

    concat1 :: [[a]] -> [a]
    concat1 [[]] = []
    concat1 [[x]] = [x]
    concat1 (h:t) = 
        h ++ concat1 t 