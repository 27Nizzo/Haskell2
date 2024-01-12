    data RTree a = R a [RTree a]

    soma :: Num a => RTree a -> a 
    soma (R a []) = a 
    soma (R a (x:xs)) = a + soma x + soma (R a xs)
    