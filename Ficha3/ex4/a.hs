type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano 
    deriving Show

type TabDN = [(Nome,Data)]

-- Defina uma função procura, que indica a data de nascimento de uma dada pessoa, caso o seu nome exista na tabela

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura [] _ = Nothing
procura n ((n2,b):t) = if n == n2 then Just b else procura n t