type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]

-- Defina a função ordena que ordena uma tabela de datas  de nascimento por ordem crescente das datas de nascimento
anterior :: Data -> Data -> Bool
anterior (D d m a) (D d1 m1 a1)
  | a < a1 && m == m1 && d == d1 = True
  | a == a1 && m < m1 = True
  | a == a1 && m == m1 && d < d1 = True
  | otherwise = False


ordena :: TabDN -> TabDN 
ordena [] = []
ordena (a:t) = insere a (ordena t)
    where insere a [] = [a] 
          insere (a,b) ((c,d):t) = if anterior b d then (a,b) : (c,d) : t
                                    else (c,d) : insere (a,b) t