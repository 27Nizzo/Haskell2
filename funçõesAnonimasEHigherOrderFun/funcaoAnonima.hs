--Def: Função sem nome ao ser definida, consiste em \, uma lista de argumentos e uma expressão como: (\<args> -> <expr>)

-- Ex1:

add1 = (\x -> x+1)

--Ex2: 
add3 = (\x y z -> x + y + z) -- 1 2 3 => 6 = 1 + 2 + 3

--Funções importantes: 

--Map: 
--mapDef :: (a -> b) -> [a] -> [b]

-- No terminal: map add1 [1,2,3] => [2,3,4], o que significa que esta função mapeia a lista toda e executa a função neste caso de add1, (x+1) em todos os elementos da lista

paresM = (\(x,y) -> x + y) -- map paresM [(1,2),(3,4)] => [3,7]

--Filter: 
--filter :: (a -> Bool) -> [a] -> [a]

maiorqDois = (\x -> x > 2) 
-- No termibal: filter maiorqDois [1,2,3,4,5,6] => [3,4,5,6], esta função vai criar uma lista do mesmo tipo da dada mas com elementos que correspondem à ação
-- neste caso se x > 2 esse x vai para uma nova lista 

paresDif = (\(x,y) -> x /= y) -- filter paresDif [(1,2),(2,2),(4,4)] => [(1,2)]
--NOTA:  em funções anonimas nao usamos "!=" para diferente e sim "/="

