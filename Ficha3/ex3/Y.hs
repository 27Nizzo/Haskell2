
data Contacto = Casa Integer 
                | Trab Integer
                | Tlm Integer
                | Email String

type Nome = String 
type Agenda = [(Nome, [Contacto])]


-- Exercicio: Verifica se um nome e contacto esta na agenda se tiver devolve a agenda como está se nao tiver vai acrescentar o novo Nome e os seus contactos

acrescenta :: Agenda -> Nome -> Contacto -> Agenda
{-acrescenta [] n c = [(n,[c])]
acrescenta [] "" c = [("", [c])]
acrescenta [] n [] = [(n,[])] -}
-- Muita coisa aqui, pode se fazer de forma mais simples como aqui em baixo 
acrescenta [] n c = [(n,[c])]
acrescenta ((x,y):t) n c | x == n = (x,y ++ [c]) : t
                         | otherwise = (x,y) : acrescenta t n c