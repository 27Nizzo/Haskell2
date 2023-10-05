-- dropa n elementos de uma lista a começar da esquerda

dropei :: Int -> [a] -> [a]
dropei 0 l = l
dropei _ [] = []
dropei n (h:t) = if (n >= length(h:t) || n < 0) then []
                    else dropei (n - 1) t
-- se n for zero entao nao tiramos nenhum elemento;
-- se qualquer n numa lista que nao tem qualquer elemento entao retornamos uma lista vazia
-- se o n for maior que o tamanho da lista retornamos uma lista vazia pois tiramos todos os elementos ou se o n for menor que 0 tambem retornamos uma lista vazia porque é impossivel tirar um numero negativo de elementos
-- se for n for maior que zero e menor que o tamnho da lista vamos tirar um a um elemento a contar da esquerda da lista ate que n seja 0,
