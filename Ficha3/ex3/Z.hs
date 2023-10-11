
data Contacto = casa Integer 
                | trab Integer
                | tlm Integer
                | email String

type Nome = String 
type Agenda = [(Nome, [Contacto])]

--Exercicio: MOstrar os nomes da agenda

Nomes :: Agenda -> [Nome]
Nomes [] = []
Nomes [n] = [n]
Nomes ((a,b):t) = a : Nomes t

