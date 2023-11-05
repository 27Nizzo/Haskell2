fromDigits :: [Int] -> Int 
formDigits [] = 0 
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigitss :: [Int] -> Int 
fromDigitss [] = 0
fromDigitss (h:t) = auxD t h 

auxD :: [Int] -> Int -> Int
auxD [] acc = acc  
auxD (h:t) acc = auxD t (h + 10*acc) 