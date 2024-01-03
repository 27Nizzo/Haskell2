--3) 
-- Alternativa à função: 

-- func :: [[Int]] -> [Int]
-- func l = concat (filter (\x -> sum x > 10) l), fazendo uma única travessia da lista l.

func :: [[Int]] -> [Int]
func [] = []
func (h:t) =
    if sum h > 10 then h ++ func t
    else func t 