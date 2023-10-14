type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]


-- defina a função idade que calcula  a idade de uma pessoa numa dada data

idade :: Data -> Nome -> TabDN ->  Maybe Int
idade _ _ [] = Nothing
idade (D d m a) nome ((n, D d1 m1 a1):t)
  | nome == n && (m > m1) || (m == m1 && d == d1) = Just (a-a1)
  | nome == n && ((m > m1) || (m == m1 && d < d1)) = Just (a-a1-1)
  | otherwise = idade (D d m a) nome t