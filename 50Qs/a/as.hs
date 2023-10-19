
--1)

enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y = if x > y then []
                    else x : enumFromTo1 (x + 1) y
--2)

enumFromThen1 :: Int -> Int -> Int -> [Int]
enumFromThen1 x y z = if x > z then []
                      else x : enumFromThen1 y (y + (y - 1)) z
--3)

maisMais :: [a] -> [a] -> [a]
maisMais [] [] = []
maisMais l [] = l
maisMais (h:t) l = h : maisMais t l

--4) 

(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) x = (!!!) t (x - 1)


--5)

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]
--6)

takee :: Int -> [a] -> [a]
takee _ [] = []
takee x (h:t) = h : takee (x - 1) t

--7)

dropp :: Int -> [a] -> [a]
dropp _ [] = []
dropp x (h:t) = if length (h:t) < x || x <= 0 then []
                else h : dropp (x - 1) t
--8) 

zipp :: [a] -> [b] -> [(a,b)]
zipp _ [] = []
zipp [] _ = []
zipp (h:t) (hs:ts) = (h,hs) : zipp t ts
--9)
replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n e = if n <= 0 then []
                 else e : replicate1 (n - 1) e

--10) 

intersperse1 :: a -> [a] -> [a]
intersperse1 _ [] = []
intersperse1 e [x] = [x]
intersperse1 e (h:t) = h : e : intersperse1 e t

--11) 

groupp :: Eq a => [a] -> [[a]]
groupp [] = []
groupp (h:t) = takee1 h (h:t) : groupp (dropp1 h t)

takee1 :: Eq a => a -> [a] -> [a]
takee1 _ [] = []
takee1 x (h:t) | h == x = h : takee1 x t
              | otherwise = []

dropp1 :: Eq a => a -> [a] -> [a]
dropp1 _ [] = []
dropp1 x (h:t) | h == x = dropp1 x t
              | otherwise = h:t

--12) 
concatt :: [[a]] -> [a]
concatt [[]] = []
concatt [[x]] = [x]
concatt (h:t) = h ++ concatt t

--13)
initss :: [a] -> [[a]]
initss [] = []
initss l = initss (retiraUltt l) ++ [l]

retiraUltt :: [a] -> [a]
retiraUltt [x] = []
retiraUltt (h:t) = h : retiraUltt t

--14) 
tailss :: [a] -> [[a]]
tailss [] = [[]]
tailss (h:t) = (h:t) : tailss t

--15)
headss :: [[a]] -> [a]
headss [] = []
headss ([]:t) = headss t
headss ((x:xs):t) = x : headss t  

--16)
totall :: [[a]] -> Int
totall [] = 0
totall (h:t) = length(h:t) + totall t

--17)
funn :: [(a,b,c)] -> [(a,c)]
funn [] = []
funn ((a,b,c):t) = (a,c) : funn t

--18)
colaa :: [(String,b,c)] -> String
colaa [] = ""
colaa ((a,b,c):t) = a ++ colaa t

--19) 
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade ano id ((x,xs):t) = if ano - xs >= id then x : idade ano id t
                          else idade ano id t

--20)
powerEnumFromm :: Int -> Int -> [Int]
powerEnumFromm 0 0 = [0]
powerEnumFromm n m = aux n m 0

aux :: Int -> Int -> Int -> [Int]
aux n m acc = if acc /= m then n^acc : aux n m (acc+1)
              else []
--21)
isPrime1 :: Int -> Bool 
isPrime1 n = isPrime1Aux n 2

isPrime1Aux :: Int -> Int -> Bool 
isPrime1Aux n m 
    | fromIntegral m <= sqrt (fromIntegral n) = if mod n m == 0 then False else isPrime1Aux n (m+1)
    | otherwise = True 

--22)
isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 [] ls = True
isPrefixOf1 l [] = False 
isPrefixOf1 (x:xs) (y:ys) = if x == y then isPrefixOf1 xs ys
                                        else False

--23) 
isSuffixOf1 :: Eq a => [a] -> [a] -> Bool
isSuffixOf1 [] [] = False
isSuffixOf1 l [] = False
isSuffixOf1 (x:xs) (y:ys) 
            | (length xs) == (length ys) = if x == y then isSuffixOf1 xs ys else False 
            | otherwise = isSuffixOf1 (x:xs) ys
            

--24) 
isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 [] ls = True 
isSubsequenceOf1 ls [] = False 
isSubsequenceOf1 (x:xs) (y:ys) = if x == y
            then isSubsequenceOf1 xs ys
            else isSubsequenceOf1 (x:xs) ys

--25)
elemIndice1 :: Eq a => a -> [a] -> [Int]
elemIndice1 n l = elemIndice1Aux n 0 l 

elemIndice1Aux :: Eq a => a -> [a] -> Int -> [Int]
elemIndice1Aux _ [] _ = []
elemIndice1Aux e (h:t) n 
                | e == h = n : elemIndice1Aux e t (n+1)
                | otherwise = elemIndice1Aux e t (n+1)

--26)

nubb :: Eq a => [a] -> [a]
nubb [] = []
nubb [x] = [x]
nubb (h:t) = pertence1 h t

pertence1 :: Int -> [Int] -> Bool
pertence1 _ [] = False
pertence1 x (h:t) = if (x == h) || pertence1 t 

--27)

delete1 :: Eq a => a -> [a] -> [a]
delete1 x [] = []
delete1 n (h:t:ts) = 
            | n == h = t: ts
            | n == t = h: ts
            | otherwise = delete1 n ts
--28)

(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) [] ys = []
(\\\) xs [] = xs
(\\\) (x:xs) ys = if elem x ys then (\\\) xs (delete1 x xs)     
                    else x : (\\\) xs (delete1 x ys)

--29)

unionn :: Eq a => [a] -> [a] -> [a]
unionn l [] = l
unionn [] l = l
unionn (x:xs) (y:ys) = if pertence2 x (h:t) then unionn (h:t) xs
                        else x : unionn xs (h:t)

pertence2 :: Eq a => a -> [a] -> Bool
pertence2 x [] = False 
pertence2 x (h:t) = if (x == h) || pertence2 x t

--30)

intersect2 :: Eq a => [a] -> [a] -> [a]
intersect2 l [] = []
intersect2 [] l = []
intersect2 (h:t) (x:y) = if pertence3 h (x:y) then h : intersect2 t (x:y)
                            else intersect2 t (x:y)

pertence3 :: Eq a => a -> [a] -> Bool
pertence3 x [] = False 
pertence3 n (h:t) = (n == h) || pertence3 n t

--31)

insert1 :: Ord a => a -> [a] -> [a]
insert1 n [] = [n]
insert n (h:t) = if n > h then h : insert1 n t
                else h : n : t

--32) 

unwordss :: [String] -> String
unwordss [] = ""
unwordss [n] = n
unwordss (h:t) = h ++ " " ++ unwordss t

--33) 

unliness :: [String] -> String
unliness [] = ""
unliness (h:t) = h ++ "\n" ++ unliness t

--34)

pMaior1 :: Ord a => [a] -> Int
pMaior1 (h:t) = let len = length t
    in let - pMaior1Aux h( len t

pMaior1Aux :: a -> Int -> [a] -> Int 
pMaior1Aux n idx (h:t) =
            if n < h then pMaior1Aux h (length t) t
            else pMaior1Aux n idx t
--35) 

lookUp1 :: Eq a => a -> [(a,b)] -> Maybe b
lookUp1 n [] = Nothing 
lookUp1 n ((a:b):t) =
        if n == a then Just b 
        else lookUp1 n t

--36) 

preCrescente1 :: Ord a => [a] -> [a]
preCrescente1 list = preCrescente1Aux [] last

preCrescente1Aux :: Ord a => [a] -> [a] -> ([a], [a])
preCrescente1Aux acc [n] = (acc ++ [n], [])
preCrescente1Aux acc (f:s:t)
    | f <= s = preCrescente1Aux (acc ++ [f]) (s:t)
    | otherwise = ((acc ++ [f]), (s:t))

--37)

iSortt :: Ord a => [a] -> [a]
iSortt [] = []
iSort (h:t) = insert1 h (iSortt t)

--38)

menorr :: String -> String -> Bool
menorr [] ys = True 
menorr xs [] = False 
menorr (x:xs) (y:ys)
        | x <= y = menor xs ys 
        | otherwise = False 

--39) 

elemSett :: Eq a => a -> [(a,Int)] -> Bool 
elemSett n [] = False 
elemSett n ((a,b):t)
        | n == a = True 
        | otherwise = elemSett n t

--40) 

convertMSett :: [(a,Int)] -> [a]
convertMSett [] = []
convertMSett (h:t) = (convertMSettAux h) ++ convertMSett t

convertMSettAux :: [a] -> [a]
convertMSettAux (a,0) = []
convertMSettAux (a,b) = aq : convertMSettAux (a,(b-1))

