posi :: [Int] -> Bool
posi [] = True 
posi (h:t) = if h > 0 then posi t else False