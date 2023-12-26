--2)
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


    --a) 
    minimo :: Ord a => BTree a -> a 
    minimo (Node x Empty _) = x 
    minimo (Node _ e _) = minimo e
    minimo Empty = error "Árvore vazia" 


    --b) 
    semMinimo :: Ord a => BTree a -> BTree a
    semMinimo Empty = Empty
    semMinimo (Node r e d) = 
            let (minElement, (Node newR newE newD)) = removeMin (Node r e d)
            in (Node newR newE newD)

    removeMin :: BTree a -> (a, BTree a)
    removeMin (Node r Empty d) = (r, d) 
    removeMin (Node r e d) = 
        let (minElement, newE) = removeMin e 
        in (minElement, Node r newE d)
    removeMin Empty = error "Árvore Vazia"

    --c) 
    minSmin :: Ord a => BTree a -> (a, BTree a)
    minSmin Empty = error "Árvore Vazia"
    minSmin (Node r Empty d) = (r, d)
    minSmin (Node r e d) =
        let (minElement, newE) = minSmin e 
        in (minElement, Node r newE d)

    --d) 
    remove :: Ord a => a -> BTree a -> BTree a 
    remove _ Empty = Empty 
    remove x (Node r e d) 
        | x == r    = removeRaiz (Node r e d)
        | x < r     = Node r (remove x e) d 
        | otherwise = Node r e (remove x d)

    removeRaiz :: Ord a => BTree a -> BTree a 
    removeRaiz (Node _ Empty d) = d 
    removeRaiz (Node _ e Empty) = e 
    removeRaiz (Node _ e d) = Node m e (remove m d)
            where m = maior e 

    maior :: BTree a -> a 
    maior (Node r _ Empty) = r 
    maior (Node _ _ d) = maior d

    removeMaior :: BTree a -> BTree a 
    removeMaior (Node _ e Empty) = e 
    removeMaior (Node r e d) = Node r e (removeMaior d)
