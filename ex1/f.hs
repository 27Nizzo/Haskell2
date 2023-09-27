--max2 – que calcula o maior de dois n´umeros inteiros.
max2 :: Int -> Int -> Int
max2 x y = if x - y > 0 
            then x
            else y
