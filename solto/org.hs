    organiza :: Eq a => [a] -> [(a,[Int])]
    -- organiza "abracadabra" = [('a',[0,3,5,7,10]),('b',[1,8]),('r',[2,4,6]),('c',[4])]
    organiza [] = []
    organiza [x] = (x, [0]) : []
    organiza l = [(x, posicoes x l)] ++ organiza (remove x l)
        where x = head l 
    
    posicoes :: Eq a => a -> [a] -> [Int]
    posicoes _ [] = []
    posicoes x l = [i | (i,y) <- zip [0..] l, x == y]

    preCrescente :: Ord a => [a] -> [a]
    preCrescente [] = []
    preCrescente (h:t:ts)
                | h < t = h : preCrescente (t:ts) 
                | h == t = preCrescente (t:ts) 
                | otherwise = [h] 

    remove :: Eq a => a -> [a] -> [a]
    remove _ [] = []
    remove n (h:t) 
                | n == h = t 
                | otherwise = h : remove n t 
