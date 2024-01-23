

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
    -- uniaoMSet [('b',2),('a',4),('c',1)] [(’c’,7),(’a’,3),(’d’,5)] corresponde a [(’c’,8),(’a’,7),(’d’,5),(’b’,2)]

    uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a
    uniaoMSet t [] = t 
    uniaoMSet [] ts = ts 
    uniaoMSet ((e1,n1):t) ((e2,n2):ts) = 
        if e1 == e2 
            then (e1, n1 + n2) : uniaoMSet t ts 
        else 
        if e1 /= e2 
            then (e1,n1) : uniaoMSet t ((e2,n2):ts)
        else 
        (e2,n2) : uniaoMSet ((e1,n1):t) ts

        