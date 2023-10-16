-- forma menos eficiente do junta

junta2 :: [a] -> [a] -> [a]
junta2 l1 [] = l1
junta l1 (h, t) = junta(append h l1) t 


append :: a -> [a] -> [a]
append x (h, t) = h : append x t