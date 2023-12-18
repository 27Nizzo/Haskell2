-- A função 'foldr' e 'foldl' ão funções de ordem superior que ajudam a reduzir uma lista para um único valor aplicando uma fução acumuladora a cada elemento de uma lista

-- Def 'foldr': a função 'foldr' aplica a função acumuladora da direita para a esquerda, ou seja, vai do fim da lista até ao inicio
    -- foldr :: (a -> b -> c) -> b -> [a] -> b
-- a -> primeiro argumento é a função acumuladora 
-- b -> segundo argumento é o valor inicial do acumaldor 
-- c -> terceiro argumento é a lista dobrada

--Ex:

soma1 :: [Int] -> Int
soma1 lista = foldr (+) 0 lista

-- soma [1,2,3,4] resulta 1+2+3+4 = 10
-- a => foldr(+)
-- b => 0 
-- c => lista
-- Neste exemplo o foldr percorre a lista [1,2,3,4] da direita para a esquerda, aplicando a função da adição (+) e acumulando os resultados. A sequencia das somas é:
-- 1 + (2 +(3 +(4 + 0)))