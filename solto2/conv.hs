    data LTree a = Tip a | Fork (LTree a) (LTree a)
    data FTree a b = Leaf a | No b (FTree a b) (FTree a b)

    conv :: LTree Int -> FTree Int Int 
    conv (Tip a) = Leaf a 
    conv (Fork e d) = No (sumLeaves e + sumLeaves d) (conv e) (conv d)

    sumLeaves :: LTree Int -> Int 
    sumLeaves (Tip a) = a 
    sumLeaves (Fork e d) = sumLeaves e + sumLeaves d 