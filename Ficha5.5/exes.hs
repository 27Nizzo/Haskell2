    import Data.List (sortBy)

--1) 

--a)
    any2 :: (a -> Bool) -> [a] -> Bool 
    any2 p = foldr (\x acc -> p x || acc) False 

--b)
    zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith2 f xs ys = foldr (\(x,y) acc -> f x y : acc) [] (zip xs ys)

--c)
    takeWhile2 :: (a -> Bool) -> [a] -> [a] 
    takeWhile2 p = foldr (\x acc -> if p x then x : acc else []) []

--d) 
    dropWhile2 :: (a -> Bool) -> [a] -> [a] 
    dropWhile2 p = foldr(\x acc -> if p x then acc else x : acc) []

--e)
    span2 :: (a -> Bool) -> [a] -> ([a],[a])
    span2 p = foldr(\x (matched, reset) -> if p x then (x : matched, reset) else (matched, x : reset)) ([],[])

--f)
    deleteBy2 :: (a -> a -> Bool) -> a -> [a] -> [a]
    deleteBy2 eq x = filter (\y -> not (eq x y))

--g)
    sortOn2 :: Ord b => (a -> b) -> [a] -> [a] 
    sortOn2 p = sortBy (\x y -> compare (p x) (p y)) 

--2)   
    type Polinomio = [Monomio]
    type Monomio = (Float, Int)
--a)
    selgrau :: Int -> Polinomio -> Polinomio
    selgrau dg = filter (\(_, exp) -> exp == dg)

--b) 
    conta :: Int -> Polinomio -> Int 
    conta dg = length . filter (\(_, exp) -> exp == dg)

--c) 
    grau :: Polinomio -> Int 
    grau polinomio = foldr (\(_, exp) acc -> max exp acc) 0 polinomio 

--d) 
    deriv :: Polinomio -> Polinomio 
    deriv polinomio = map (\(x,exp) -> (x * fromIntegral exp,exp - 1)) polinomio

--e) 
    calcula :: Float -> Polinomio -> Float 
    calcula x = foldr (\(xs,exp) acc -> acc + xs * (x ^ exp)) 0.0
-- calcula 3 [(1,2)] = 3^2 = 9 

--f) 
    simp :: Polinomio -> Polinomio 
    simp polinomio = filter (\(coef, exp) -> exp /= 1) polinomio
--simp [(1,2),(1,1),(2,1)] = [(1,2)]

--g) 
    mult :: Monomio -> Polinomio -> Polinomio 
    mult (coefM, expM) polinomio = map (\(coef, exp) -> (coef * coefM, exp + expM)) polinomio

--h) 
    ordena :: Polinomio -> Polinomio 
    ordena polinomio = sortBy (\(_,exp1) (_,exp2) -> compare exp1 exp2) polinomio

--i) 
    normaliza :: Polinomio -> Polinomio 
    normaliza l = let x = fromIntegral $ grau l in [((a/x),b) | (a,b) <- l]

--j) 
    soma :: Polinomio -> Polinomio -> Polinomio 
    soma polinomio1 polinomio2 = normaliza $ combinaPolinomios polinomio1 polinomio2

    combinaPolinomios :: Polinomio -> Polinomio ->  Polinomio 
    combinaPolinomios polinomio1 polinomio2 = foldr inserir polinomio1 polinomio2
        where   
            inserir :: (Float, Int) -> Polinomio -> Polinomio 
            inserir x acc = x : filter (\(coef, exp) -> exp /= snd x) acc

-- ou
    soma2 :: Polinomio -> Polinomio -> Polinomio 
    soma2 pol1 pol2 = normaliza $ (++) pol1 pol2 

--k) 
    produto :: Polinomio -> Polinomio -> Polinomio 
    produto pol1 pol2 = normaliza $ concatMap (multPorMonomio pol1) pol2

    multPorMonomio :: Polinomio -> (Float, Int) -> Polinomio 
    multPorMonomio pol (coefM, expM) = map(\(coef, exp) -> (coef * coefM, exp + expM)) pol 

--l) 
    equiv :: Polinomio -> Polinomio -> Bool 
    equiv pol1 pol2 = ordena(normaliza pol1) == ordena(normaliza pol2)

--3) 

    type Mat a = [[a]]
  
{-
Por exemplo, a matriz (triangular superior):

[1 2 3]
[0 4 5]
[0 0 6], seria representada por: [[1,2,3], [0,4,5], [0,0,6]]
-}

--a) 
    dimOk :: Mat a -> Bool 
    dimOk (l:rl) = all(\l1 -> length l == length l1) rl

--b) 
    dimMat :: Mat a -> (Int,Int)
    dimMat (l:rl) = (length l, length (l:rl)) 

--c) 
    addMat :: Num a => Mat a -> Mat a -> Mat a 
    addMat m1 m2 = zipWith2 (\l1 l2 -> zipWith2 (+) l1 l2) m1 m2
    
--d) 
    transpose' :: Mat a -> Mat a
    transpose' ([]:_) = []
    transpose' m = 
            let l = map head m 
                rl = map tail m 
            in l : transpose' rl 

--e) 
    multMat :: Num a => Mat a -> Mat a -> Mat a
    multMat m1 m2 = zipWith2 (\l1 l2 -> zipWith2 (*) l1 l2) m1 m2

--f) 
    zipMat  :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
    zipMat f m1 m2 = zipWith2 (\l1 l2 -> zipWith2 f l1 l2) m1 m2

--g) 

--??

    triSup :: (Num a, Ord a) => Mat a -> Bool
    triSup mat = all isZeroBelowDiagonal mat
     where
    isZeroBelowDiagonal row = and $ zipWith (\j x -> if j < length row then x <= 0 else True) [0..] row
    
--??