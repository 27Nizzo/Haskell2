data Btree a = Empty 
                | Node a (Btree a) (Btree a) deriving Show

a,a1,a2 :: Btree Int
a = Node 30 a1 a2
a1 = Node 20
            (Node 10 Empty Empty) Empty
a2 = Node 64 (Node 45 
                    (Node 40 Empty Empty)
                    (Node 50 Empty Empty))
                        (Node 78 Empty Empty)

prune :: Int -> Btree a -> Btree a
prune 0 _ = Empty
prune _ Empty = Empty 
prune n (Node r e d) = Node r (prune (n-1) e) 
                               (prune (n-1) d)

