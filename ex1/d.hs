--multiplo – tal que multiplo m n testa se o n´umero inteiro m ´e m´ultiplo de n
multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n == 0
                then True
                else False
                