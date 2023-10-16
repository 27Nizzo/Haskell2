parteAoMeio2 :: [a] -> ([a],[a])
parteAoMeio2 [] = ([],[])
parteAoMeio2 [x] = ([x],[])
parteAoMeio2 (h:t) = (h : b,a)
    where (a,b) = parteAoMeio2 t