data Btree a = Empty 
                | Node a (Btree a) (Btree a)

a,a1,a2 :: Btree Int
a = Node 30 a1 a2
a1 = Node 20
            (Node 10 Empty Empty) Empty
a2 = Node 64 (Node 45 
                    (Node 40 Empty Empty)
                    (Node 50 Empty Empty))
                        (Node 78 Empty Empty)


procura :: Ord a => a -> Btree a -> Bool
procura _ Empty = False 
procura x (Node r e d) 
                        | x == r = True
                        | x < r = procura x e
                        | otherwise = procura x d
