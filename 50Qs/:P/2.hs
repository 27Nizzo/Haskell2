enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 x y z = if x > z then []
                                else x : enumFromThenTo1 y (y +(y - x)) z


    -- 1 2 10 = [1,2,3,4,5,6,7,8,9,10]
    -- se x for maior que zero é impossivel ou seja lista vazia;
    -- Então quando o x for menor que o z, a sequencia começa no x e vai saltando com a subtração entre y e x até ser menor ou igual a z