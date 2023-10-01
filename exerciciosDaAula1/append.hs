-- função q junta um elemento a uma lista

append1 :: a -> [a] -> [a]
append1 x [] = [x]
append x (h, t) = h : append1 x t 