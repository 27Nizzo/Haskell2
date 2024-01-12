    data RTree a = R a [RTree a] 
        deriving Show 
    
    prune :: Int -> RTree a -> RTree a 
    prune 0 (R a _) = R a []
    prune n (R a (x:xs)) = R a (map (prune (n-1))(x:xs))
    