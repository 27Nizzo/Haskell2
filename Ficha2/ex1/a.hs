funA :: [Double] -> Double
funA [] = 0
funA (h:t) = h^2 + (funA t)


-- O resultado da lista [2,3,5,1] é 39
-- A funA pega na head da lista e eleva ao quadrado e soma 
-- , soma com o quadrado da head da tail, por exemplo:
-- funA [2,3,5,1] = 2^2 + (funA [3,5,1])
--                = 4 + 3^2 + (funA[5,1])
--                = 4 + 9 + 25 + (funA[1])
--                = 13 + 25 + 1^2 + (funA[0])
--                = 38 + 1 + 0
--                = 39