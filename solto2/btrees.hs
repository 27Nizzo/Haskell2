    data BTree a = Empty 
                  | Node a (BTree a) (BTree a)
                deriving Show

    minimo :: Ord a => BTree a -> a
    minimo (Node r Empty _) = r 
    minimo (Node _ e _) = minimo e 

    semMinimo :: Ord a => BTree a -> BTree a 
    semMinimo Empty = Empty 
    semMinimo (Node _ Empty d) = d 
    semMinimo (Node r e d) = Node r (semMinimo e) d 

    minSmin :: Ord a => BTree a -> (a,BTree a)
    minSmin (Node r Empty d) = (r,d)
    minSmin (Node r e d) = (mini, Node r e' d)
                    where (mini, e') = minSmin e 

    remove :: Ord a => a -> BTree a -> BTree a
    remove _ Empty = Empty
    remove x (Node r ae ad) | x > r = Node r ae (remove x ad)
                            | x < r = Node r (remove x ae) ad
                            | otherwise = aux ad ae
                                where aux :: Ord a => BTree a -> BTree a -> BTree a
                                      aux Empty ad = ad
                                      aux ae Empty = ae
                                      aux ae ad = Node m ae ad'
                                            where (m , ad') = minSmin ad 
