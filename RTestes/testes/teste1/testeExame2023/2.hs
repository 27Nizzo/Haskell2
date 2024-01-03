--2) 
-- Defina uma instˆancia da classe Eq para o tipo Caminho, considerando iguais os 
-- caminhos com a mesma posi¸c˜ao de partida e de chegada e com o mesmo 
-- n´umero de movimentos

     type Posicao = (Int,Int) 
     data Movimento = Norte | Sul | Este | Oeste 
     data Caminho = C Posicao [Movimento]

        instance Eq Caminho where 
            (C, (x,y) xs) == (C, (x1,y1) ys) = (x == x1) && (y == y1) && (xs == ys)
            