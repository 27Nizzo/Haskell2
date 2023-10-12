
data Contacto = Casa Integer
| Trab Integer
| Tlm Integer
| Email String
deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]
