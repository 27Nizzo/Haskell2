-- função q verifica se um elemento dado pertence à lista
-- a função elem tambem é uma função já definida

elem1 :: Eq a => a -> [a] -> Bool
elem1 x (h,t) = if x == h then True 
                            else if elem x t
                                then True
                                else False