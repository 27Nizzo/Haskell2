data Contacto = Casa Integer
| Trab Integer
| Tlm Integer
| Email String
deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- Exercicio: Dado um nome e uma agenda retorna o numero de telefone de casa se existir

casa :: Nome -> Agenda -> Maybe Integer     
casa "" agenda = 0 
casa n ((x,(y:ys):t)) = 