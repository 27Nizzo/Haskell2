numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:t) = if c == h then 1 + numOcorre c t
                              else numOcorre c t