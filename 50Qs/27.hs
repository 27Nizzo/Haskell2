deletee :: Eq a => a -> [a] -> [a]
deletee _ [] = []
deletee n (h:t:ts) = if n == h then t:ts 
                                else if n == t then h : ts     
                                else deletee n ts