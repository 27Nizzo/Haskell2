    data RTree a = R a [RTree a]
        deriving Show 

    mirror :: RTree a -> RTree a 
    mirror (R a []) = (R a [])
    mirror (R a (x:xs)) = R a (map mirror (reverse (x:xs)))
    