-- DEfine a função total, que recebe uma lista de listas e conta o total de elementos 
-- totall [[1,2,3],[4,5]] = 5

totall :: [[a]] -> Int
totall [] = 0
totall (x:t) = length x + totall t