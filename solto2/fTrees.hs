    data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
    data LTree a = Tip a | Fork (LTree a) (LTree a)
    data LTree a = Tip a | Fork (LTree a) (LTree a)



    splitFTree :: FTree a b -> (BTree A, LTree b)
    splitFTree Leaf b = (Empty, Tip b)
    splitFTree (No r e d) = 
            ( 
                (Node r(fst(splitFTree e)) (fst(splitFTree d))), (Fork (snd(splitFTree e)) (snd(splitFTree d))))
     
     {-joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
     joinTrees Empty (Tip a) = Just (Leaf a) 
     joinTrees (Node r e d) (Fork e1 d1) = 
        Just No r aux1 aux2 
            where aux1 = joinTrees e e1 
                  aux2 = joinTrees d d1

    joinTrees _ _ = Nothing -}
