-- defina a função fun que recebe uma lista de triplos e produz a lista de pares com o primeiro e o terceiro triplo

funn :: [(a,b,c)] -> [(a,c)]
funn [] = []
funn ((a,b,c):t) = (a,c) : funn t