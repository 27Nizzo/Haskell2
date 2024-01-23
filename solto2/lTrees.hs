    data BTree a = Empty | Node a (BTree a) (BTree a)
    data LTree a = Tip a | Fork (LTree a) (LTree a)


    ltSum :: Num a => LTree a -> a 
    ltSum (Tip a) = a 
    ltSum (Fork e d) = ltSum e + ltSum d

    listaLT :: LTree a -> [a] 
    listaLT (Tip a) = [a] 
    listaLT (Fork e d) = listaLT e ++ listaLT d 

    ltHeight :: LTree a -> Int 
    ltHeight (Tip a) = 1 
    ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

    -- LTree/Leaf Tree é uma árvore binária por isso usamos a função max invés de maximum 

    