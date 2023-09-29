--a)Defina a fun¸c˜ao next :: Semaforo -> Semaforo que calcula o pr´oximo estado de um sem´aforo.

data Semaforo = Verde | Amarelo | Vermelho deriving(Show,Eq)

next :: Semaforo -> Semaforo
next a  = case a of
            Verde -> Amarelo
            Amarelo -> Vermelho
            otherwise -> Verde