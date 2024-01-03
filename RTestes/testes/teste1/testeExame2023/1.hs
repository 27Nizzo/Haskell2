--1) 
--a) 
    type MSet a = [(a,Int)]

    -- convertMSet [('b',2), ('a', 4), ('c', 1)] = 'bbaaaac'

    convertMSet :: MSet a -> [a]
    convertMSet [] = []
    convertMSet [(_, 0)] = []
    convertMSet ((h,t):ts) = replicate t h ++ convertMSet ts 

--b) 
    --removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),(’a’,4)]

    --removeMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),(’a’,3), (’c’,1)]

    removeMSet :: Eq a => a -> MSet a -> MSet a
    removeMSet _ [] = [] 
    removeMSet n ((h,t): ts) = 
            if n == h && t == 1 then ts 
            else if n == h && t > 1 then (h,t-1) : ts 
            else (h,t) : removeMSet n ts 

        
--c) 
    --  [(’c’,7),(’a’,3),(’d’,5)] corresponde a [(’c’,8),(’a’,7),(’d’,5),(’b’,2)]

    uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a
    uniaoMSet xs [] = xs 
    uniaoMSet [] ys = ys 
    uniaoMSet ((h,hs):xs) ((t,ts):ys) =
        if h == t then (hs + ts) : uniaoMSet hs ts 
        else if h < t then (h,hs) : uniaoMSet xs ((t,ts):ys) 
        else (t,ts) : uniaoMSet ((h,hs):xs) ys 