-- outra forma para reverter uma lsita

reverse2 :: [a] -> [a] 
reverse2 l = l
reverse2 l  = (last l) : reverse2(init l)

-- ele pega no último elemento de l mete-o como primeiro elemento em uma nova lista e dps vai buscar o reverso do resto da lista
-- last é uma função predefinida assim como a função init