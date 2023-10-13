--defina a função heads que recebe uma lista de listas e produz a lista com o primeiro elemento de cada lista
-- headss [[2,3,4],[1,7],[],[8,5,3]] = [2,1,8]


headss :: [[a]] -> [a]
headss [] = []
headss ([]:t) = headss t
headss ((x:xs):t) = x : headss t