data Contacto = Casa Integer
    | Trab Integer
    | Tlm Integer
    | Email String
deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- Exercicio: Uma função que com um dado nome acrescenta na agenda a informação do email

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail "" "" agenda = agenda  
acrescEmail x y agenda = [x ,[Email y]] ++ agenda 

