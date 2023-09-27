-- c)Defina a fun¸c˜ao safe :: Semaforo -> Semaforo -> Bool que testa se o estado de dois sem´aforos num cruzamento ´e seguro

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

safe :: Semaforo -> Semaforo -> Bool
safe a b = if a == Amarelo && b == Verde 
            then True
            else if a == Verde && b == Vermelho
                then False
                else if a == Vermelho && b == Amarelo
                    then False
                    else False