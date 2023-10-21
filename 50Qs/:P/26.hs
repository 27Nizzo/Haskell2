nubb :: Eq a => [a] -> [a]
nubb [] = []
nubb [x] = [x]
nubb (h:t) = if pertence h t then nubb t 
             else h : nubb t

pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence n (h:t) = (n == h) || pertence n t