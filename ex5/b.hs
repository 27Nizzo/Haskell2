--b) Defina a fun¸c˜ao stop :: Semaforo -> Bool que determina se ´e obrigat´orio parar num sem´aforo

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

stop :: Semaforo -> Bool
stop n = if n == Vermelho
            then True