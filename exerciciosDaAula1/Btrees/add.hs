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

add :: Ord a => a -> Btree a -> Btree a 
add x Empty = Node x Empty Empty 
add x (Node r e d) = if x <= r then Node r (add x e) d
                     else Node r e (add x d)