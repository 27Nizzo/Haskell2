    data FTree a b = Leaf b | No a (FTree a b) ( FTree a b)
    data BTree a = Empty | Node a (BTree a) (BTree a)
    data LTree a = Tip a | Fork (LTree a) (LTree a)


    --a) Função que separa uma árvore com informação nos nodos, como também nas folhas

    splitFTree :: FTree a b -> (BTree a, LTree b) 
    splitFTree  (Leaf b) = (Empty, Tip b)
    splitFTree (No r e d) = ((Node r (fst(splitFTree e)) (fst(splitFTree d))), (Fork (snd(splitFTree e)) (snd(splitFTree d))))


    --b) Função que sempre que as árvores sejam compativeis as junta numa só 
    
    joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
    joinTrees (Empty) (Tip a) = Just (Leaf a) 
    joinTrees (Node r e d) (Fork e1 d1) = Just (No r aux1 aux2)
                    where Just aux1 = joinTrees e e1 
                          Just aux2 = joinTrees d d1 
    joinTrees _ _ = Nothing 
