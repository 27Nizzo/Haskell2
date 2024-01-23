    data RTree a = R a [RTree a]

    soma :: Num a => RTree a -> a 
    soma (R a [])= a 
    soma (R a (h:t)) = a + soma h + soma (R a t)

    altura :: RTree a -> Int 
    altura (R a []) = 1 
    altura (R a l) = 1 + maximum(map altura l)

    -- maximum é uma função que procura o maximo / o maior valor em listas não binarias 
    -- max é uma função que procura o maximo / o maior valor em listas binarias

    prune :: Int -> RTree a -> RTree a 
    prune 0 (R a []) = R a []
    prune n (R a l) = R a (map(prune(n-1)) l)