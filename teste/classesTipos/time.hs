data Time = AM Int Int | PM Int Int | Total Int Int

-- Função que converte um valor Time para minutos
timeToMinutes :: Time -> Int
timeToMinutes (AM h m) = h * 60 + m
timeToMinutes (PM h m) = (h + 12) * 60 + m
timeToMinutes (Total h m) = h * 60 + m

-- Definindo a instância da classe Eq para a comparação de horas
instance Eq Time where
  t1 == t2 = timeToMinutes t1 == timeToMinutes t2
