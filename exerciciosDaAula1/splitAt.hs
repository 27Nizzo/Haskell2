splitAt1 :: Int -> [a] -> ([a],[a])
splitAt1 _ [] = ([],[])
splitAt1 0 l = ([], l)
splitAt1 n (h:t) = 
            let (x,y) = splitAt1 (n - 1) t 
            in (h:x,y)
