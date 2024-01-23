    data BTree a = Empty 
                  | Node a (BTree a) (BTree a)
            deriving Show 

    type Aluno = (Numero, Nome, Regime, Classificacao)
    type Numero = Int 
    type Nome = String 
    data Regime = ORD | TE | MEL deriving Show 
    data Classificacao = Aprov
                        | Rep 
                        | Faltou
        deriving Show 
    type Turma = BTree Aluno 


    inscNum :: Numero -> Turma -> Bool 
    inscNum _ Empty = False 
    inscNum n (Node (num,_,_,_) e d) | n == num = True 
                                     | n > num = inscNum n d 
                                     | otherwise = inscNum n e 

    inscNome :: Nome -> Turma -> Bool 
    inscNome  _ Empty = False 
    inscNome "" _ = True 
    inscNome n (Node (_,nome,_,_) e d) 
                        | n == nome = True 
                        | otherwise =  inscNome n e || inscNome n d 

    trabEst :: Turma -> [(Numero, Nome)] 
    trabEst Empty = [] 
    trabEst (Node (num,nome,te,_) e d) = (num,nome) : ((trabEst e) ++ (trabEst d)) 

    nota :: Numero -> Turma -> Maybe Classificacao 
    nota _ Empty = Nothing 
    nota n (Node (num,nome,te,c) e d) = 
        if n == num then Just c 
        else if n < num then nota n e 
        else nota n d

