enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 0 0 = [0]
enumFromTo1 x y = x : enumFromTo1(x+1) y