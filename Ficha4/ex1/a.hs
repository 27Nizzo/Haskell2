import Data.Char (isDigit, isAlpha)
     
digitAlpha1 :: String -> (String,String)
digitAlpha1 "" = ("", "")
digitAlpha1 (h:t) = if isAlpha h then (h:r1,r2)
                                    else if isDigit h then (r1,h:r2)
                                    else (r1,r2)
        where (r1,r2) = digitAlpha1 t         


{-isDigitt :: Char -> Bool
isDigitt n = if (n >= '0' && n <= '9')
            then True
            else False

isAlphaa :: Char -> Bool
ĩsAlphaa n = if (n >= 'A' && n <= 'Z') || (n >= 'a' && n <= 'z')
            then True 
            else False-}

