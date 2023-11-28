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
    deleteBy' 