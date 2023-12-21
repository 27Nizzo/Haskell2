    data RTree a = R a [RTree a]

    elemR :: Eq a => a -> RTree a -> Bool
    elemR x (R r l) = x == r || any (elemR x) l

    elementos :: RTree a -> [a]
    elementos (R r l) = r : concatMap elementos l
