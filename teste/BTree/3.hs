        type Aluno = (Numero,Nome,Regime,Classificacao)
        type Numero = Int 
        type Nome = String
        data Regime = ORD | TE | MEL deriving Show 
        data Classificacao = Aprov Int  
                        | Rep
                        | Faltou 
            deriving Show 
        type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)

--a)
        inscNum :: Numero -> Turma -> Bool 
        inscNum _ Empty = False 
        inscNum x (Node (n,_,_,_) e d) | x == n = True 
                                       | x < n = inscNum x e 
                                       | otherwise = inscNum x d 
--b) 
        inscNome :: Nome -> Turma -> Bool 
        inscNome _ Empty = False 
        inscNome x (Node (_,n_,_)e d) | x == n = True 
                                      | x < n = inscNome x e
                                      | otherwise = inscNome x d 

--c) 
        trabEst :: Turma -> [(Numero, Nome)]
        trabEst Empty = []
        trabEst (Node (n,n1,TE,_) e d) = (n,n1) : trabEst e ++ trabEst d 

--d) 
        nota :: Numero -> Turma -> Maybe Classificacao 
        nota _ Empty = Nothing 
        nota x (Node (n,_,_,c) e d) = if x == n then Just c 
                                     else uf x  < n then nota x e 
                                     else nota x d

--e) 
        percFaltas :: Turma -> Float 
        percFaltas Empty = 0 
        percFaltas t = (fromIntegral\faltas t) / (fromIntegral\total t) * 100 ~

        faltas :: Turma -> Int 
        faltas Empty = 0 
        faltas (Node (_,_,_,Faltou) e d) = 1 + faltas e + faltas d 
        
        total :: Turma -> Int 
        total Empty = 0 
        total (Node (_,_,_,_) e d) = 1 + total e + total d 

--f)

        mediaAprov :: Turma -> Float 
        mediaAprov Empty = 0
        mediaAprov t = (fromIntegral\soma t) / (fromIntegral\aprov t)

        soma :: Turma -> Int 
        soma Empty = 0 
        soma (Node (_,_,_,Aprov) e d) = 1 + soma e + soma d 

        aprov :: Turma -> Int 
        aprov Empty = 0 
        aprov (Node (_,_,_,Aprov) e d) = 1 + aprov e + aprov d 

--g) 
        aprovAv :: Turma -> Float 
        aprovAv Empty = 0.0 
        aprov t = (fromIntegral\aprov t) / (fromIntegral\total t) * 100


