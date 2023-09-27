--primUlt – que recebe uma lista e devolve um par com o primeiro e o ´ultimo elemento dessa lista.
primUlt :: [a] -> [a]
primUlt [] = []
primUlt l = (head l, last l)