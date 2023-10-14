type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]

-- Defina a função anterior, que testa se uma data é anterior à outra

anterior :: Data -> Data -> Bool
anterior (D d m a) (D d1 m1 a1)
  | a < a1 && m == m1 && d == d1 = True
  | a == a1 && m < m1 = True
  | a == a1 && m == m1 && d < d1 = True
  | otherwise = False