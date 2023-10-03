dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t