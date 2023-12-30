--3) 
    data BTree a = Empty | Node a (BTree a) (BTree a)
    data LTree a = Tip a | Fork (LTree a) (LTree a)

--a) Soma as folhas de uma árvore
    ltSum :: Num a => LTree a -> a 
    ltSum (Tip a) = a
    ltSum (Fork l r) = ltSum l + ltSum r 

--b) Lista as folhas de uma árvore 
    listaLT :: LTree a -> [a]
    listaLT (Tip a) = [a]
    listaLT (Fork l r) = listaLT l ++ listaLT r 

--c) Calcula a altura de uma árvore 
    ltHeight :: LTree a -> Int  
    ltHeight (Tip a) = 1
    ltHeight (Fork l r) = 1 + max (ltHeight l) (ltHeight r)
    