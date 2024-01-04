    randomSel :: Int -> [a] -> IO [a] 
-- randomSel 3 [1,3,1,4,2,8,9,5]] poderia produzir qualquer uma das listas [1,4,2], [5,2,8] ou [1,9,1], mas nunca [2,3,2].

    randomSel 0 _ = return []
    randomSel n l = do 
            i <- randomRIO (0, length l - 1)
            r <- randomSel (n-1) (take i l ++ drop (i+1) l)

            return ((l!!i):r)

    randomRIO :: (Int,Int) -> IO Int 
    randomRIO (a,b) = do 
            r <- randomRIO (a,b)
            return r 