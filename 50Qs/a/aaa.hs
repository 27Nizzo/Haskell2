delete1 :: Eq a => a -> [a] -> [a]
delete1 _ [] = []
delete1 n (h:t:ts) 
    | n == h = t:ts
    | n == t = h:ts
    | otherwise = delete1 n ts