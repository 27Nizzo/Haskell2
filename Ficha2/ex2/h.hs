nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros e [] = False
nosPrimeiros e ((a,b):t) = if e == a then True
                                     else False