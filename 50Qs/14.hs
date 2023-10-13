-- Apresenta uma definição da função tails que calcula o numero de sufixos de uma lista
--tailss [1,2,3] ? [[1,2,3],[2,3],[3],[]]

tailss :: [a] -> [[a]] 
tailss [] = [[]]
tailss (h:t) = [(h:t)] ++ tailss t