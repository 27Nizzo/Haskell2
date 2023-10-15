type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]
-- Defina a função poridade, que apresenta o nome e a idade das pessoas, numa dada data por ordem crescente da idade das pessoas

poridade :: Data -> TabDN -> [(Nome,Int)]
poridade _ [] = []
poridade (D d m a) l = (nome, idade) : poridade (D d m a) ts
    where ((n, D dx mx ax):ts) = ordena l
    idade = if m > mx || mx == m && d > dx then (a - ax) else ((a - ax)- 1)



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