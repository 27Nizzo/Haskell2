    data BTree a = Empty 
                | Node a (BTree a) (BTree a)
            deriving Show



    -- Árvore balanceada
    a1 = Node 1 
        (Node 2 
            (Node 4 Empty Empty) 
            (Node 5 Empty Empty)
        ) 
        (Node 3 
            (Node 6 Empty Empty) 
            Empty
        )

-- Árvore não balanceada
    a2 = Node 1 
        (Node 2 
            (Node 3 
                (Node 4 Empty Empty) 
                Empty
            ) 
            Empty
        ) 
        Empty

    --Arvore balenciada:

    balance :: BTree a -> Bool 
    balance Empty = True 
    balance (Node r e d) = abs(altura e - altura d) <= 1 && balance e && balance d 

    altura :: BTree a -> Int 
    altura Empty = 0
    altura (Node r Empty Empty) = 1 
    altura (Node r e d) = 1 + max (altura e) (altura d)

