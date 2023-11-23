-- Defina cada uma das listas seguintes por compreens˜ao.
-- (a) [1,2,4,8,16,32,64,128,256,512,1024]

exp2 = [2^x | x <- [0..10]]
--R: esta lista corresponde os valores de 2^0 a 2^10

-- (b) [(1,5),(2,4),(3,3),(4,2),(5,1)]

sum6 = [(x,y) | x <- [1..10], y <- [1..10], x+y == 6]
-- R: Os pares onde a sua soma é igual a 6

-- (c) [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]

min6 = [[y | y <- [1..x]] | x <- [1..5]]
min6' = [[1..x] | x <- [1..5]]
-- R: Cria uma lista de listas de valores menores que 6

-- (d) [[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]

list = [replicate x 1 | x <- [1..5]]
-- R: Lista de Listas do valor 1

-- (e) [1,2,6,24,120,720]

prod = [ product [y | y <- [1..x]] | x <- [1..6]]
