    data Lista a = Esq a (Lista a) | Dir a (Lista a) | Nula 

{- 3) Consider o seguinte tipo de dados para representar uma lista em que os elementos podem ser 
acrescentados à esquerda(Esq) ou à direita(Dir) da lista. Nula representa a lista vazia
-}
   
--a) Funçãp que recebe uma lista nao vazia r devolve a lista sem o seu elemento mais a direita
    semUltimo (Esq _ Nula) = Nula 
    semUltimo (Dir Nula _) = Nula 
    semUltimo (Esq x xs) = Esq x (semUltimo xs) 
    semUltimo (Dir x xs) = xs 
    semUltimo Nula = Nula 

--b) Defina Lista como instˆancia da classe Show de forma a que a lista 
    -- Esq 1 (Dir (Dir (Esq 9 Nula) 3) 4) seja apresentada como [1, 9, 3, 4].

    instance Show a => Show (Lista a) where
        show Nula = "[]"
        show (Esq x Nula) = show x
       -- show (Dir Nula x) = show x 
        show (Esq x xs) = show x ++ ", " ++ showLista xs 
        show (Dir x xs) = show xs ++ ", " ++ show x 

    showLista :: Show a => Lista a -> String 
    showLista = init . tail . show 
    