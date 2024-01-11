--defina uma função que converte uma árvore binaria para uma rose tree

    data RTree a = R a [RTree a]
        deriving Show 

    data BTree a = Empty 
                | Node a (BTree a) (BTree a)
            deriving Show

    --a)
    binToRT :: BTree a -> RTree a
    binToRT Empty = error "Árvore vazia"
    binToRT (Node r Empty Empty) = R r []
    binToRT (Node r Empty d) = R r [binToRT d]
    binToRT (Node r e Empty) = R r [binToRT e]
    binToRT (Node r e d) = R r [binToRT e, binToRT d]
    
         