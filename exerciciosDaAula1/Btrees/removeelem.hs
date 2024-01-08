{-Exercício: Com base nesta ideia, defina uma função que remove um elemento de uma árvore 
de procura. Comece por definir uma função que devolve um par com o mínimo de uma árvore 
não vazia e a árvore sem o mínimo
-}

    data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show 

    a1 = Node 1 
        (Node 2 
            (Node 4 Empty Empty) 
            (Node 5 Empty Empty)
        ) 
        (Node 3 
            (Node 6 Empty Empty) 
            Empty
        )

    elemrem :: Ord a => a -> BTree a -> BTree a 
    elemrem _ Empty = Empty 
    elemrem x (Node r e d) | x < r = Node r (elemrem x e) d 
                           | x > r = Node r e (elemrem x d)
                           | otherwise = remove (Node r e d) 

    remove :: Ord a => BTree a -> BTree a 
    remove (Node r Empty d) = d 
    remove (Node r e Empty) = e 
    remove (Node r e d) = Node m e (elemrem m d)
        where m = minimo d 

    minimo :: Ord a => BTree a -> a 
    minimo (Node r Empty d) = r 
    minimo (Node r e d) = minimo e 

