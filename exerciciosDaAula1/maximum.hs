-- função que encontra o maior valor de uma lista
-- a função max tambem ja esta predefinida ele compara dois elementos e verifica qual dos dois é o maior

maximum1 :: Ord a => [a] -> a
maximum1 [x] = x
maximum1 (x:(y:t)) = maximum1((max x y) t)
