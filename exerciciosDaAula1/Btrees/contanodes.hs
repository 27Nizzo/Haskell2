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

--Conta os nodes de uma árvore

contanodes :: Btree a -> Int 
contanodes Empty = 0
contanodes (Node r e d) = 1 + contanodes e + contanodes d