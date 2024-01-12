    data RTree a = R a [RTree a]
        deriving Show 
    
    postOrder :: RTree a -> [a]
    postOrder (R a []) = [a]
    postOrder (R a (x:xs)) = postOrder x ++ postOrder (R a xs) 
    