    data RTree a = R a [RTree a] 
    type Dictionary = [RTree (Char, Maybe String)]

    insere :: String -> String -> Dictionary -> Dictionary 
    insere [] _ d = d 
    insere (h:t) s d = insereAux h t s d 
    
    insereAux :: Char -> String -> String -> Dictionary -> Dictionary   
    insereAux _ _ [] d = d 
    insereAux c (h:t) s [] = [R (c, Nothing) (insereAux h t s [])] 
    insereAux c (h:t) s ((R (x,y) l):xs) 
        | c == x = (R (x, Just s) l) : xs 
        | otherwise = (R (x,y) l) : insereAux c (h:t) s xs 
