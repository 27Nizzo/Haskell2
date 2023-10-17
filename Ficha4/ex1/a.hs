import Data.Char (isDigit, isAlpha)

digitAlphaa :: String -> (String,String)
digitAlphaa "" = ("","")
digitAlphaa (h:t) = if isAlphaa h then (h: r1,r2)
                                 else if isDigitt h then (r1,r2)
                                 else (r1,r2)
    where (r1,r2) = digitAlphaa t

isDigitt :: Char -> Bool
isDigitt n = if (n >= '0' && n <= '9')
            then True
            else False

isAlphaa :: Char -> Bool
ĩsAlphaa n = if (n >= 'A' && n <= 'Z') || (n >= 'a' && n <= 'z')
            then True 
            else False