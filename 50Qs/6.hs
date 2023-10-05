-- Das o numero de elementos que queres retirar de uma lista, ou seja 1 [1,2,3] vai tirar um elemento a começar pela esquerda

take2 :: Int -> [a] -> [a]
take2 _ [] = []
take2 x (h:t) = if x <= 0 then []
                        else h : take2 (x-1) t
                     