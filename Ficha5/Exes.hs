--1) Apresente defini¸c˜oes das seguintes fun¸c˜oes de ordem superior, j´a pr´e-definidas no Prelude ou no Data.List

--a)
    any' :: (a -> Bool) -> [a] -> Bool 
    -- any odd [1..10] == True
    any' f [] = False
    any' f (h:t) = f h || any' f t 

--b) 
    zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
    -- zipWith' (+) [1,2,3,4] [10,20,30,40] == [11,22,33,44]
    zipWith' f (h:t) (hs:ts) = h `f` hs : zipWith' f t ts
    zipWith' f _ _ = []

--c) 
    takeWhile' :: (a -> Bool) -> [a] -> [a]
    -- takeWhile' odd [1,3,4,5,6,6] == [1,3]
    takeWhile' f [] = []
    takeWhile' f (h:t) = if f h then h : takeWhile' f t else []

--d) 
    dropWhile' :: (a -> Bool) -> [a] -> [a]
-- dropWhile' odd [1,3,4,5,6,6] == [4,5,6,6]
    dropWhile' f [] = []
    dropWhile' f (h:t) = if f h then dropWhile' f t else (h:t)

--e) 
    span' :: (a -> Bool) -> [a] -> ([a],[a])
-- span' p l = (takeWhile' p l, dropWhile' p l)
    span' f [] = ([],[])
    span' f (h:t) | f h = (h:a,b)
                  | otherwise = ([], (h:t))
                  where (a,b) = span' f t

--f) 
    deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
    deleteBy' f _ [] = []
    deleteBy' f n (h:t) = if f n h then t else h : deleteBy' f n t

--g) 
    sortOn' :: Ord b => (a -> b) -> [a] -> [a]
    sortOn' f [] = [] 
    sortOn' f (h:t) = aux f h (sortOn' f t)
                    where aux :: Ord b => (a -> b) -> a -> [a] -> [a]
                          aux f x [] = [x]
                          aux f x (a:b) = if f x > f a then a : aux f x b else x : a : b

--2) 

    type Polinomio = [Monomio]
    type Monomio = (Float, Int)

    --Por exemplo: [(2,3), (3,4), (5,3), (4,5)] = 2x^3 + 3x^4 + 5x^3 + 4x^5

--a) Seleciona os graus de um polinómio

    selgrau :: Int -> Polinomio -> Polinomio 
    selgrau n [] = []
    selgrau n pol = filter(\x -> n == snd (x)) pol
-- a função filter é uma função de ordem superior que recebe uma função de predicado e uma lista. Ela retorna uma lista. Ela retorna uma nova lista contendo 
-- apenas os elementos da lista de entrada para os quais o predicado é verdadeiro

--b) Conta quantos monómios existem em um poilinómio especifico

    conta :: Int -> Polinomio -> Int 
    conta n pol = foldr(\x conta -> if n == snd(x) then conta+1 else conta) 0 pol 
    
    conta' :: Int -> Polinomio -> Int 
    conta' n p = length(filter (\x -> n == snd x) p)
-- a função foldr é uma função de ordem superior que é usada para reduzir uma lista para um único valor. Ela toma uma função binária e um valor inicial
-- como argumentos, junto com a lista que se deseja dobrar.

--c) Diz o maior grau existente em um polinómio 

    grau :: Polinomio -> Int 
    grau pol = foldr(\x g -> if snd x > g then  snd(x) else g) 0 pol 

--d) Calcula a derivada de um polinómio 

    deriv :: Polinomio -> Polinomio 
    deriv pol = let l = map(\(c,g) -> if g > 0 then (c*fromIntegral(g),g-1) else (0,0)) pol             
                in filter(/=(0,0)) l 

--e) Calcula o valor de um polinómio para um valor especifico de x 

    calcula :: Float -> Polinomio -> Float
    calcula x pol = foldr(\(c,g) soma -> c*(x^g) + soma) 0 pol 

--f) Polinómio que retira de um polinómio os monómios de coeficiente zero

    simp :: Polinomio -> Polinomio
    simp pol = filter(\x -> fst(x) /= 0) pol

--g) Calcula o resultado da multiplicação de um monómio por um polinómio 

    mult :: Monomio -> Polinomio -> Polinomio
    mult (cm,gm) pol = map(\x -> (fst(x)*cm, snd(x)*gm)) pol 

--h) Ordena o polináomio por ordem crescente dos graus dos seus monomios

    ordena :: Polinomio -> Polinomio
    ordena pol = sortOn' snd pol 
    

    ordena' :: Polinomio -> Polinomio 
    ordena' pol = foldr zux [] pol 
            where aux :: Monomio -> Polinomio -> Polinomio
                  aux (cm,gm) [] = [(cm,gm)]
                  aux (cm,gm) ((cm2,gm2):t) = if gm < gm2 then (cm,gm) : (cm2,gm2) : t else (cm2,gm2) : aux (cm,gm) t
                  
--i) Normaliza um polinómio
 
    normaliza :: Polinomio -> Polinomio 
    normaliza l = let x = fromIntegral $ grau l in [((a/x),b) | (a,b) <- l]



