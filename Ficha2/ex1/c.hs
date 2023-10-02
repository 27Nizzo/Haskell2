funC :: [a] -> [a]
funC (x:y:t) = funC t
funC[x] = [x]
funC [] = []
-- esta função recebe uma lista e dá o último elemento da mesma.
--Por exemplo: funC [1,2,3,4,5] = [5]
