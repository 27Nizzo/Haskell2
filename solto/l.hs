    intersperse1 :: a -> [a] -> [a]
    intersperse1 _ [] = []
    intersperse1 _ [x] = [x]
    intersperse1 n (h:t) = h : n : intersperse1 n t

    groupp :: Eq a => [a] -> [[a]]
    groupp [] = []
    groupp [x] = [[x]]
    groupp (h:t) = 
        takee h (h:t) : groupp (dropp h t)

    takee :: Eq a => a -> [a] -> [a]
    takee _ [] = []
    takee x (h:t) 
        | x == h = h : takee x t 
        | otherwise = []
    
    dropp :: Eq a => a -> [a] -> [a]
    dropp _ [] = []
    dropp x (h:t) 
        | x == h = dropp x t 
        | otherwise = h:t 

    concat1 :: [[a]] -> [a]
    concat1 [[]] = []
    concat1 [[x]] = [x]
    concat1 (h:t) = 
        h ++ concat1 t 
    
    initss :: [a] -> [[a]]
    initss [] = [[]]
    initss l = initss (retiraUlt l) ++ [l]

    retiraUlt :: [a] -> [a]
    retiraUlt [x] = []
    retiraUlt (h:t) = h : retiraUlt t

    tailss :: [a] -> [[a]]
    tailss [] = [[]]
    tailss (h:t) = [(h:t)] ++ tailss t

    headss :: [[a]] -> [a]
    headss [] = []
    headss [[x]] = [x]
    headss ([]:t) = headss t
    headss ((h:t):ts) = h : headss ts  

    total :: [[a]] -> Int 
    total [] = 0 
    total (h:t) = length(h) + total t 

    fun :: [(a,b,c)] -> [(a,c)]
    fun [] = []
    fun ((a,b,c):t) = (a,c) : fun t 

    cola :: [(String,b,c)] -> String 
    cola [] = ""
    cola ((a,b,c):t) = a ++ cola t

    idade :: Int -> Int -> [(String,Int)] -> [String]
--idade 2021 26 [("rui",1995), ("maria",2009), ("ana",1947)] corresponde a ["rui","ana"]
    idade _ _ [] = []
    idade x y ((a,b):t) = 
                if x - b >= y then a : idade x y t
                else idade x y t

    powerEnumFrom :: Int -> Int -> [Int]
    powerEnumFrom x y = 
        if x > y then []
        else x : powerEnumFrom (x^2)y

    isPrime :: Int -> Bool 
    isPrime 0 = False 
    isPrime 1 = True
    isPrime n = 
        if length [x | x <- [2..n-1], mod n x == 0] > 0 then False 
        else True 

    isPrefixOf1 :: Eq a => [a] -> [a] -> Bool 
    isPrefixOf1 [] _ = True 
    isPrefixOf1 _ [] = False 
    isPrefixOf1 (h:t) (x:xs) = 
        if h == x then isPrefixOf1 t xs 
        else False 
    
    isSuffixOf1 :: Eq a => [a] -> [a] -> Bool
    isSuffixOf1 [] _ = True 
    isSuffixOf1 _ [] = False 
    isSuffixOf1 l1 l2 = 
        if last l1 == last l2 then isSuffixOf1 (init l1) (init l2)
        else False 

