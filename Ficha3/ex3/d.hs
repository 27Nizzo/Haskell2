data Contacto = Casa Integer
                | Trab Integer
                | Tlm Integer
                | Email String
    deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- Exercicio: Dado um nome e uma agenda retorna o numero de telefone de casa se existir
casa :: Nome -> Agenda -> Maybe Integer
casa n [] = Nothing
casa n ((x, contacts) : t) =
    if n == x
        then case findCasa contacts of
            Just (Casa num) -> Just num
            _ -> Nothing
        else casa n t
  where
    findCasa :: [Contacto] -> Maybe Contacto
    findCasa [] = Nothing
    findCasa (c:cs) = case c of
        Casa num -> Just c
        _ -> findCasa cs
