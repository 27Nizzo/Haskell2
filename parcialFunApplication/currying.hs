-- A utilização do currying é quando temos este tipo de funções:
f :: a -> b -> c -> d 
-- ou seja, uma função que recebe 3 argumentos e retorna um valor
-- com o currying podemos escrever de outra forma equivalente: 
fs :: a -> (b -> (c -> d)) -- isto resulta de o argumento a resulta no argumento b, onde o argumento b resulta no argumento c e por último 
-- o argumento c resulta no valor final de d
--Ex: 

add :: Int -> Int 
add x y = x + y 
add x = (\y -> x + y)
add = (\x - > (\y -> x + y))
-- estas 3 funções são equivalentes 
