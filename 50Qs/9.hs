-- Constroi uma lista com n elementos todos iguais a x elementos

replicate2 :: Int -> a -> [a] 
replicate2 n x 
        | n <= 0 = []
        | otherwise = x : replicate2 (n - 1)x

        -- o n é o numero de vezes q o numero vai ser repetido e o x o número q vai ser repetido n vezes;
        -- se o n for menor ou igual a zero nao devolve uma lista vazia
        -- se for maior q zero ele adiciona x à cabeça da lista e chama outra vez a função replicate2 subtraindo o n por um fazendo outra vez o mesmo processo até chegar a 0