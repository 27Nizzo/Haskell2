    data RTree a = R a [RTree a]
        deriving Show
    
    altura :: RTree a -> Int 
    altura (R a []) = 1
    altura (R a (x:xs)) = 1 + max (altura x) (altura (R a xs))