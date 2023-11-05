nzpp :: [Int] -> (Int, Int, Int) 
nzpp [] = (0,0,0)
nzpp (x:xs) | x == 0 = (a,b+1,c)
            | x < 0 = (a+1,b,c)
            | x > 0 = (a,b,c+1)
    where (a,b,c) = nzpp xs