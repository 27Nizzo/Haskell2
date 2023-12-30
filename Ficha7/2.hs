--2)
    
    data RTree a = R a [RTree a]

--a) Soma os elementos da árvore 
    soma :: Num a => RTree a -> a 
    soma (R a []) = a 
    soma (R a l) = a + sum (map soma l)

--b) Calcula a altura de uma árvore
    altura :: RTree a -> Int 
    altura (R a []) = 1
    altura (R a l) = 1 + maximum (map altura l)

--c) Remove de uma árvore todos os elementos a partir de uma determinada profundidade
    prune :: Int -> RTree a -> RTree a
    prune 0 (R a l) = R a []
    prune n (R a l) = R a map(prune (n-1) l)

--d) Gera uma árvore simétrica
    mirror :: RTree a -> RTree a 
    mirror (R a []) = R a [] 
    mirror (R a l) = R a (reverse (map mirror l))

--e) travessia postOrder da árvore 
    postOrder :: RTree a -> [a]
    postOrder (R a []) = [a]
    postOrder (R a l) = concat (map postOrder l) ++ [a]
     