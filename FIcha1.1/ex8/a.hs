--ord :: Char -> Int
--chr :: Int -> Char

isLower :: Char -> Bool 
isLower c = if (c >= 'a' && c <= 'z')
            then True
            else False