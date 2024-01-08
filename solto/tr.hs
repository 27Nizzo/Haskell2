
        enumFromTo1 :: Int -> Int -> [Int]
        enumFromTo1  x y 
            | x > y = []
            | y == 0 = []
            | otherwise = x : enumFromTo1 (x+1) y 

        enumFromThenTo1 :: Int -> Int -> Int -> [Int] 
        enumFromThenTo1 x y z = 
            if x > z then []
            else if y == 0 then []
            else x : enumFromThenTo1 (x + (y - 1)) y z 

        maisMais :: [a] -> [a] -> [a]
        maisMais xs [] = xs 
        maisMais [] ys = ys 
        maisMais (h:t) (x:xs) = h : maisMais t (x:xs) 

        posicao :: [a] -> Int -> a 
        posicao (h:t) x 
                 | x == 0 = h 
                 | otherwise = posicao t (x - 1) 

        reverse' :: [a] -> [a]
        reverse' [] = [] 
        --reverse' (h:t) = reverse' t ++ [h]
        reverse' l = last l : reverse' (init l)

        take1 :: Int -> [a] -> [a]
        take1 _ [] = []
        take1 0 l = []
        take1 x (h  :t) = 
                    if x > length (h:t) then (h:t)
                    else h : take1 (x - 1) t

        drop1 :: Int -> [a] -> [a]
        drop1 _ [] = []
        drop1 0 l = l 
        drop1 x (h:t) 
                    | x > length (h:t) = []
                    | otherwise = drop (x-1) t

        zipp :: [a] -> [b] -> [(a,b)]
        zipp [] _ = []
        zipp _ [] = []
        zipp (h:t) (x:xs) = (h,x) : zipp t xs

        replicate' :: Int -> a -> [a]
        replicate' 0 _ = []
        replicate' num n = n : replicate' (num - 1) n


        intersperse1 :: a -> [a] -> [a]
        intersperse1 _ [] = []  -- Caso base: se a lista fornecida for vazia, retorna uma lista vazia.
        intersperse1 _ [x] = [x]  -- Caso base: se a lista fornecida contiver apenas um elemento, retorna a própria lista.
        intersperse1 sep (x:xs) = x : sep : intersperse1 sep xs
-- Passo recursivo: para uma lista com pelo menos dois elementos, retorna o primeiro elemento, seguido pelo separador,
-- e depois pela chamada recursiva para o restante da lista.

        

   


    