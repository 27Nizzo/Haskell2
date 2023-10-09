ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((a,x):t) = insere (a,x) (ordena t)
    where insere (a,x) [] = (a,x)
          insere (a,x1) ((b,x2):t) = if x1 <= x2 then ((a,x1):(b,x2):t)
                                     else (b,x2): insere (a,x1) t
