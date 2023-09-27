--c) converter um valor em horas (par de inteiros) para minutos (inteiro)
--1 hora -> 60 min
--recebes dois inteiros (x1,y1) e converte x1 para minutos e y1 deixa estar e dps somas x1 + y1

conv :: Int -> Int
conv x1 = x1 * 60

convhor :: (Int, Int) -> Int
convhor (x1,y1) = conv(x1) + y1 