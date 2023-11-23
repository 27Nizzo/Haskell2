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

folhas :: Btree a -> [a]
-- folhas a = [10, 40, 50, 78]
folhas Empty = []
folhas (Node r Empty Empty) = [r]
folhas (Node r e d) = folhas e ++ folhas d