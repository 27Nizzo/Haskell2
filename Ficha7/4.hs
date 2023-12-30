--4)
    
    data BTree a = Empty | Node a (BTree a) (BTree a)
    data LTree a = Tip a | Fork (LTree a) (LTree a)
    data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

    --a) Separa uma árvore com informação nos nodos e nas folhas em duas árvores de tipos diferentes.

    splitFTree :: FTree a b -> (BTree a, LTree b)
    splitFTree (Leaf b) = (Empty, Tip b) 
    splitFTree (No a l r) = (Node a (fst (splitFTree l)) (fst (splitFTree r)), 
                            Fork (snd (splitFTree l)) (snd (splitFTree r)))

    --b) Sempre que as árvores sejam compatíveis as junta em uma só
    
    joinTree :: BTree a -> LTree b -> Maybe (FTree a b)
    joinTree (Empty) (Tip b) = Just (Leaf b) 
    joinTree (Node r ae1 ad1) (Fork ae2 ad2) = Just (No r aux1 aux2)
                                                where Just aux1 = joinTree ae1 ae2 
                                                      Just aux2 = joinTree ad1 ad2 
    joinTree _ _ = Nothing 

