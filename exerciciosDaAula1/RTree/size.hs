    data RTree a = R a [RTree a]

size :: RTree a -> Int 
size (R r []) = 1
size (R r (h:t)) = (size h) + (size (R r t))
 