nubb :: Eq a => [a] -> [a] 
nubb [] = [] 
nubb (h:t) 
    | h `elem` t = nubb t 
    | otherwise = h : nubb t 