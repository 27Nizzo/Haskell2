enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y = if x <= y then enumFromTo1 (x+1) y
                            else []