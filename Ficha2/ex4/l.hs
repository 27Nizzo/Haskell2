equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv p1 p2 | ordena(normaliza p1) == ordena(normaliza p2) = True
            | otherwise = False