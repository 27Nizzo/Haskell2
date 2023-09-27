--max3 – que calcula o maior de trˆes n´umeros inteiros, usando a fun¸c˜ao max2
max2 :: Int -> Int -> Int
max2 x y = if x - y > 0 
            then x
            else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (x (max2 y z))