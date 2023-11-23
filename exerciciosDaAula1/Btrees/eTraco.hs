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

-- Verifica se o caule pertence à folha ou vice versa

eTraco :: Eq a => [a] -> Btree a -> Bool
eTraco [] Empty = True
eTraco [] _ = False 
eTraco (h:t) Empty = False 
eTraco (h:t) (Node r e d) = (h == r) && (eTraco t e || eTraco t d)