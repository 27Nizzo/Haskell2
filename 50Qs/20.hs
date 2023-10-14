-- apresente a definição de uma função que dado um valor n e um valor m constroi uma lista [n^0,(...),n^m-1]

powerEnumFrom1 :: Int -> Int -> [Int]
powerEnumFrom1  0 0 = [0]
powerEnumFrom1 n m = aux n m 0

aux :: Int -> Int -> Int -> [Int]
aux n m acc = if acc /= m then n^acc : aux n m (acc + 1)   
                          else []


-- powerEnumFrom1 2 3 = [0,2,4] = [2^0,2^1,2^2]