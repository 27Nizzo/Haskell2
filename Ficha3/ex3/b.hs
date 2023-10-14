data Contacto = Casa Integer
    | Trab Integer
    | Tlm Integer
    | Email String
 deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]


--EXercicio: Dado um nome e uma agenda, retorna a lista dos emails associados a esse nome. Se esse nome não existir na agenda a função deve retornar Nothing

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails "" [] = Nothing 
verEmails n ((x,(y:ys):t)) = if n == x then Just (emails (x:xs))
                                        else verEmails n t
emails :: [Contacto] -> [String]
emails [] = []
emails (x:xs) = case x of Email c -> cs : emails xs 
                otherwise -> emails xs