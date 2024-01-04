    type Mat a = [[a]]

    soma :: Num a => Mat a -> Mat a -> Mat a 
    soma [] [] = []
    soma [] ys = ys 
    soma xs [] = xs
    soma (h:t) (h1:t1) = (somaV h h1) : soma t t1 

    somaV :: Num a => [a] -> [a] -> [a]
    somaV [] [] = []
    somaV (h:t) (h1:t1) = (h+h1) : somaV t t1 
