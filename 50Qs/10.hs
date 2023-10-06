-- um dado elemento e uma lista constroi uma lista em que o elemento é fornecido é intercalado netre os elementos da lista fornecida
-- interspace2 1 [10,20,30] = [10,1,20,1,30]

interspace2 :: a -> [a] -> [a]
interspace2 _ [] = []
interspace2 n [x] = [x]
interspace2 n (h:t) = h : t : interspace2 n t 