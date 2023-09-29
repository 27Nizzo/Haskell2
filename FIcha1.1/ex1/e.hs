--truncaImpar – que recebe uma lista e, se o comprimento da lista for ´ımpar retira-lhe o primeiro elemento, caso contr´ario devolve a pr´opria lista.
truncaImpar :: [a] -> [a]
truncaImpar [] = []
truncaImpar l = if mod (length l) 2 == 0 
                then l 
                else tail l