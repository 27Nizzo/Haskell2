import GHC.ST (STret)

data Movimento = Credito Float | Debito Float
    deriving Show

data Data = D Int Int Int
    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

--Defina a fun¸c˜ao filtro :: Extracto -> [String] -> [(Data,Movimento)] que retorna informa¸c˜ao relativa apenas aos movimentos cuja descri¸c˜ao esteja inclu´ıda na lista fornecida no segundo parˆametro

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro _ [] = []
filtro (Ext x1 ((a,b,c):t)) l = if b `elem` l then (a,c) : filtro (Ext x1 t) l
                                else filtro (Ext x1 t) l