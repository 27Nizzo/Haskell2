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

--3)
