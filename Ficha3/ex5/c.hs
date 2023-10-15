data Movimento = Credito Float | Debito Float
    deriving Show

data Data = D Int Int Int
    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

crebDeb :: Extracto -> (Float,Float)
crebDeb (Ext x1 ((a,b,c):t)) = case c of 
        Credito c -> (c + cre, deb)
            where (cre, deb) = crebDeb (Ext x1 t)
        Debito c -> (cre, c + deb) 
            where (cre,deb) = crebDeb (Ext x1 t)
