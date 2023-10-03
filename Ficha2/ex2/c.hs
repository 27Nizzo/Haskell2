positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h < 0 then False
                    else positivos t

--eu quero checkar se todos os elementos de uma lista são verdadeiros. Começamos por checkar se a head é par, se for passamos para os passo seguinte se não é falsa. 