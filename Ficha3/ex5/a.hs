data Movimento = Credito Float | Debito Float 
    deriving Show

data Data = D Int Int Int   
    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

-- COnstrua a função extValor, que produz uma lista de todos os movimentos superiores a um determinado valor

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext x1 ((a,b,c):t)) x2 | x1 > x2 = c : extValor (Ext x1 t) x2 
                                 | otherwise = extValor (Ext x1 t) x2
                                    