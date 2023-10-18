
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

