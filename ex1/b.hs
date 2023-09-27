-- dist – que calcula a distˆancia entre dois pontos no plano Cartesiano. Cada ponto é um par de valores do tipo Double.
dist :: (Int , Int) -> (Int, Int) -> Double
dist (x1, y1) (x2, y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)