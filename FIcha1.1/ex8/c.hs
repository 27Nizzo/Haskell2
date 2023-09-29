isAlpha :: Char -> Bool
isAlpha c = if (c >= 'A' && c >= 'Z') || (c >= 'a' && c <= 'z')
            then True 
            else False  