import Data.ByteString (tails, isSuffixOf)
import Data.List (isSubsequenceOf)
import Distribution.Types.GenericPackageDescription.Lens (_Arch)
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 0 0 = []
enumFromTo1 x y = if y <= 0 && y < x then []
                    else x : enumFromTo1 (x+1) y

enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x y z = if y <= 0 && x > z then []
                        else x : enumFromThenTo1 (x + y - 1) y z

maisMais :: [a] -> [a] -> [a]
maisMais [] l = l 
maisMais l [] = l
maisMais (h:t) l = h : maisMais t l 


(!!!) :: [a] -> Int -> a 
(!!!) (h:t) 0 = h
(!!!) (h:t) x = (!!!) t (x-1)


reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 [x] = [x]
reverse1 (h:t) = reverse1 t ++ [h]


take1 :: Int -> [a] -> [a]
take1 0 l = []
take1 x (h:t) = h : take1 (x-1) t


drop1 :: Int -> [a] -> [a]
drop1 0 l = l 
drop1 x (h:t) = h : drop1 (x-1) t


zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 (a:as) (b:bs) = (a,b) : zip1 as bs


replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n e = e : replicate1 (n-1) e


intersperse1 :: a -> [a] -> [a]
intersperse1 n [] = [n]
intersperse1 n (h:t) = h : n : intersperse1 n t


groupp :: Eq a => [a] -> [[a]]
groupp [] = []
groupp [x] = [[x]]
groupp (h:t) = takee h (h:t) : groupp dropp (h t)

takee :: Eq a => a -> [a] -> [a]
takee _ [] = []
takee x (h:t) | h == x  = h : takee x t
              | otherwise = []

dropp :: Eq a => a -> [a] -> [a]
dropp _ [] = []
dropp x (h:t) | h == x = dropp x t
              | otherwise = h : t


concatt :: [[a]] -> [a]
concatt [] = []
concatt (h:t) = h ++ concatt t


initss :: [a] -> [[a]]
initss [] = [[]]
initss ls = initss(init ls) ++ [ls]


tailss :: [a] -> [[a]]
tailss [] = [[]]
tailss [] = [(h:t)] ++ tailss t


headss :: [[a]] -> [a]
headss [[]] = []
headss([]:t) = headss t
headss (h:t) = head h : headss t


totall :: [[a]] -> Int 
totall [] = 0
totall (h:t) = length (h:t) + totall t


funn :: [(a,b,c)] -> [(a,c)]
funn [] = []
funn ((a,b,c):t) = (a,c) : funn t


colaa :: [(String,b,c)] -> String 
colaa [] = ""
colaa ((a,b,c):t) = a ++ colaa t


idade :: Int -> Int -> [(String,Int)] -> [String]
ĩdade _ _ [] = []
idade x y ((a,b):t) | x - b >= y = a : idade x y t 
                    | otherwise = idade x y t


powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom 0 0 = [0]
powerEnumFrom = powerAux n m 0

powerAux :: Int -> Int -> Int -> [Int]
powerAux n m acc = if m /= acc then n^acc : powerAux n m (acc + 1)
    else []


isPrime :: Int -> Bool
isPrime 0 = False 
isPrime n = isPrimeAux n 2

isPrimeAux :: Int -> Int -> Bool 
isPrimeAux n m | fromIntegral m <= sqrt(fromIntegral) = if mod n m == 0 then False 
            else isPrimeAux n (m+1)
            | otherwise = True


isPrefixOff :: Eq a => [a] -> [a] -> Bool
isPrefixOff [] l = True 
isPrefixOff l [] = False
isPrefixOff (x:xs) (y:ys) =
        if x == y then isPrefixOff xs ys
        else False


isSuffixOff :: Eq a => [a] -> [a] -> Bool
isSuffixOff [] [] = True
isSuffixOff [] l = False 
isSuffixOff (x:xs) (y:ys) 
      | (length xs) == (length ys) = if x == y then isSuffixOff xs ys
      else False 
      | otherwise = isSuffixOff (x:xs) ys


isSubsequenceOff :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOff l [] = True 
isSubsequenceOff [] l = False 
isSubsequenceOff (x:xs) (y:ys)
    | x == y = isSubsequenceOff xs ys
    | otherwise = isSubsequenceOff (x:xs) ys


elemIndicess :: Eq a => a -> [a] -> [Int] 
elemIndicess _ [] = []
elemIndicess n list = elemIndicessAux n list 0

elemIndicessAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicessAux _ [] _ = []
elemIndicessAux e (h:t) n = if e == h then n : elemIndicessAux e t (n+1)
else elemIndicessAux e t (n+1)


nubb :: Eq a => [a] -> [a]
nubb [] = []
nubb (h:t) = if pertence h t then nubb t
else h : nubb t

pertence :: Eq a => a -> [a] -> Bool
pertence _ [] = False
pertence n (h:t) = (n == h) || pertence t


deletee :: Eq a => a -> [a] -> [a]
deletee _ [] = []
deletee n (h:t:ts) = if n == h then t : deletee t
else if n == t then h : deletee ts
else deletee ts


(\\\) :: Eq a => [a] -> [a] ->[a]
(\\\) l [] = l
(\\\) [] _ = []
(\\\) (a:b) (h:t) = (\\\) (deletee1 h (a:b)) t 

deletee1 :: Eq a => a -> [a] -> [a]
deletee1 _ [] = []
deletee1 n (h:t) = if n == h then t
    else h : deletee1 n t