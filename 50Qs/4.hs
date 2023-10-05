-- Calcula a posição de um inteiro numa lista, exemplo: [1,2,3] 2 = 1

posi :: [a] -> Int -> a
posi (h:t) 0 = h
posi (h:t) x = posi t (x - 1)


