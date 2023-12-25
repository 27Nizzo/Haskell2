import Data.Maybe

--Ex: 
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv a b = 
        if b == 0 then Nothing else Just $ div a b 

-- funções que temos a nosso dispor a partir de import Data.Maybe 

isJust :: Maybe a -> Bool 

isNothing :: Maybe a -> Bool -- estas dua verificam se um valor é um 'Just' ou um 'Nothing', estas funções são boas na utilização das funções "map" e "filter"

formJust :: Maybe a -> a 

fromMaybe :: Maybe a -> a 