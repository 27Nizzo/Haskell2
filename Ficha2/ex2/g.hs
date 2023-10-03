segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t