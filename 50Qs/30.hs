intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 l [] = []
intersect1 [] l = []
intersect1 (h:t) (x:xs) = if pertence h (x:xs) then h : intersect1 t (x:xs)
                         else intersect1 t (x:xs)


pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence n (h:t) = (n == h) || pertence n t