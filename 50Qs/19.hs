-- defina a função idade que recebe o ano, a idade e uma lista  de pares com o nome e o ano de nascimento de cada pessoa, e devolve a lista de nomes de pessoas que nesse ano atingirão ou ja ultrapassaram a idade correspondente

idade :: Int -> Int -> [(String,Int)] -> [String]
ĩdade _ _ [] = []
idade x y ((a,b):t) | x - b >= y = a : idade x y t 
                    | otherwise = idade x y t

-- 2021 - 2009 = 12 < 26 -> entao nao aparece
-- 2021 - 1995 = 26 = 26 -> entao aparece 