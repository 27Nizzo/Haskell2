parteAoMeio1 :: [a] -> ([a],[a])
parteAoMeio1 [] = ([],[])
parteAoMeio1 [x] = ([x],[])
parteAoMeio1 (x:y:t) = (x:xs, y:ys) 
    where (xs,ys) = parteAoMeio1 t