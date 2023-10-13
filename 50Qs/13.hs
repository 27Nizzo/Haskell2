-- APresenta uma função init que calcula a lista dos prefixos de uma lista
-- inits[11,21,13] = [[], [11], [11,21], [11,21,13]]

initss :: [a] -> [[a]]
initss [] = []
initss (h:t) = h ++ initss (h:t) 

