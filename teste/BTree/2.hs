
    data BTree a = Empty | Node a (BTree a) (BTree a)
        deriving Show 

    a1 = Node 1 
        (Node 2 
            (Node 4 Empty Empty) 
            (Node 5 Empty Empty)
        ) 
        (Node 3 
            (Node 6 Empty Empty) 
            Empty
        )

    minimo :: Ord a => BTree a -> a 
    minimo (Node r Empty d) = r 
    minimo (Node r e d) = minimo e 
---------
    semMinimo :: Ord a => BTree a -> BTree a 
    semMinimo Empty = Empty 
    semMinimo (Node r Empty d) = Empty 
    semMinimo (Node r e d) = Node r (semMinimo e) d 
   
-- c) Defina a função minSmin :: Ord a => BTree a -> (a, BTree a) que calcula, com uma unica travessia da árvore o resultado
-- das duas funções anteriores 

    minSmin :: Ord a => BTree a -> (a, BTree a)
    minSmin (Node r Empty d) = (r,d) 
    minSmin (Node r e d) = (m, Node r e d')
        where (m, d') = minSmin e 

--d) define remove, que remove um elemento de uma árvore de procura, usando a função minSmin

    remove :: Ord a => a -> BTree a -> BTree a 
    remove _ Empty = Empty 
    remove x (Node r e d) | x < r = Node r (remove x e) d 
                          | x > r = Node r e (remove x d)
                          | otherwise = Node m e d' 
                          where (m, d') = minSmin d 


