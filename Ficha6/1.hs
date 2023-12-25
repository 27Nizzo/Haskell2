    data BTree a = Empty 
                | Node a (BTree a) (BTree a)
            deriving Show 

--Arvore de exemplo: 
    arvoreExemplo :: BTree Int 
    arvoreExemplo = Node 30 a1 a2 

    a1 :: BTree Int 
    a1 = Node 20 (Node 10 Empty Empty) Empty 

    a2 :: BTree Int 
    a2 = Node 64 (Node 45 (Node 40 Empty Empty) (Node 50 Empty Empty))
             (Node 78 Empty Empty)

-- a) 
    altura :: BTree a -> Int 
    altura Empty = 0
    altura (Node r e d) = 1 + max (altura e) (altura d)

-- b)
    contaNodos :: BTree a -> Int 
    contaNodos Empty = 0 
    contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d 

-- c) 
    folhas :: BTree a -> Int 
    folhas Empty = 0
    folhas (Node r Empty Empty) = 1
    folhas (Node r e d) = folhas e + folhas d

-- d) 
    prune :: Int -> BTree a -> BTree a 
    prune _ Empty = Empty 
    prune 0 _ = Empty 
    prune n (Node r e d) = Node r (prune (n - 1) e) (prune (n - 1) d)

-- e) 
    path :: [Bool] -> BTree a -> [a]
    path _ Empty = []
    path [] (Node r _ _) = [r]
    path (False:xs) (Node _ e _ ) = path xs e 
    path (True:xs) (Node _ _ d) = path xs d 

-- f) 
    mirror :: BTree a -> BTree a 
    mirror Empty = Empty 
    mirror (Node r e d) =  Node r (mirror e) (mirror d)

-- g) 
    zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
    zipWithBT _ Empty _ = Empty 
    zipWithBT _ _ Empty = Empty 
    zipWithBT n (Node r1 e1 d1) (Node r2 e2 d2) = 
            Node(n r1 r2) (zipWithBT n e1 e2) (zipWithBT n d1 d2)

-- h) 
    unzipBT :: BTree (a, b, c) -> (BTree a, BTree b, BTree c)
    unzipBT Empty = (Empty, Empty, Empty)
    unzipBT (Node (r1,r2,r3) e d) = 
        (Node r1 eA dA, Node r2 eB dB, Node r3 eC dC)
            where (eA, eB, eC) = unzipBT e 
                  (dA, dB, dC) = unzipBT d 