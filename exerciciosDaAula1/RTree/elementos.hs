    data RTree a = R a [RTree a]

    elementos :: RTree a -> [a]
    elementos (R r l) = r : concat (map elementos l) 
    