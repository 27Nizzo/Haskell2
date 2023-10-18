isPrime' :: Int -> Bool
isPrime' n = isPrime'aux n 2

isPrime'aux :: Int -> Int -> Bool
isPrime'aux n m
    | fromIntegral m <= sqrt (fromIntegral n) = if mod n m == 0 then False else isPrime'aux n (m+1)
    | otherwise = True


                    