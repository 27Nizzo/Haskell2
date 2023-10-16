mergeSort1 :: [a] -> [a]
mergeSort1 [] = []
mergeSort1 [x] = [x]
mergeSort1 l = 
         let (a,b) = parteAoMeio l 
         in merge1 (mergeSort1 a) (mergeSort1 b)

merge1 :: Ord a => [a] -> [a] -> [a]
merge1 [] ys = ys
merge1 xs [] = xs
merge1 (x:xs) (y:ys) = if x <= y then x : merge1 xs (y:ys)
                        else y : merge1 (x:xs) ys

parteAoMeio :: [a] -> ([a],[a])
parteAoMeio l = 
                let n = length l 
                    x = div n 2
                    a = take1 x l 
                    b = drop1 x l
                    in (a,b)

take1 :: Int -> [a] -> [a]
take1 _ [] = []
take1 0 l = l
take1 n (h:t) = h : take (n - 1) t

drop1 :: Int -> [a] -> [a]
drop1 _ [] = []
drop1 0 l = l
drop1 n (h:t) = h : drop1 (n - 1) t