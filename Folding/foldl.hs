-- foldl : A função 'foldl' aplica a função acumuladora da esquerda para a direita, ou seja, começa da esquerda para a direita 

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- b -> primeiro argumento é a função acumuladora 
-- a -> segundo argumento é o valor inicial do acumulador 
-- b -> terceiro argumento é a lista a ser dobrada

--Ex: 
soma1 :: [Int] -> Int 
soma1 lista = foldl (+) 0 lista
--soma [1,2,3,4] resulta em 10 


-- Neste exemplo, 'foldl' percorre a lista [1,2,3,4] da esquerda para a direita, aplicando a função da adição (+) e acumula os resultados que foram precurridos. A operação 
-- final é: (((0 + 1)+ 2)+ 3)+ 4 
