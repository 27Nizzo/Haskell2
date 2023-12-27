--3) 
    data BTree a = Empty
          | Node a (BTree a) (BTree a)
        deriving Show

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
    inscNum x (Node (n,_,_,_) e d)
      | x == n = True 
      | x > n = inscNum x d 
      | otherwise = inscNum x e 
      -- Se o numero for maior que o numero da raiz, vai para a direita, se for menor vai para a esquerda.
      -- Até encontrar o numero que seja igual ao numero de um aluno existente na árvore, ou até chgar
      -- a um ponto onde a folha seja vazia, ou seja, o numero nao existe na árvore.

      --b)
    inscNome :: Nome -> Turma -> Bool 
    inscNome _ Empty = False 
    inscNome s (Node(_,n,_,_) e d) = 
            if n == s then True 
            else inscNome s e || inscNome s d 


     --c) 
    trabEst :: Turma -> [(Numero, Nome)] 
    trabEst Empty = []
    trabEst (Node (n, nome, TE, _) e d) = (n, nome) : trabEst e ++ trabEst d 
    -- simplesmente vai aos aluns TE e vai buscar o numero e o nome e dps junta a lista dos alunos

    --d)
    nota :: Numero -> Turma -> Maybe Classificacao
    nota _ Empty = Nothing 
    nota x (Node (n,_,_,c) e d) 
        | x == n = Just c
        | x > n = nota x d 
        | otherwise = nota x e 

    --e) 
    percFaltas :: Turma -> Float 
    percFaltas Empty = 0 
    percFaltas turma = (faltaram / totalDeAlunos) * 100 
        where (faltaram, totalDeAlunos) = contarFaltas turma 

    contarFaltas :: Turma -> (Float, Float)
    contarFaltas Empty = (0,0)
    contarFaltas (Node aluno e d) = 
        let (faltaramE, totalE) = contarFaltas e 
            (faltaramD, totalD) = contarFaltas d 
        in case aluno of 
            (_,_,_,Faltou) -> (1 + faltaramE + faltaramD, 1 + totalE + totalD) 
            (_,_,_,_) -> (faltaramE + faltaramD, 1 + totalE + totalD)   

    --f)
    mediaAprov :: Turma -> Float 
    mediaAprov Empty = 0 
    mediaAprov turma = (somaDasNotas / totalDeAlunos)
        where (somaDasNotas, totalDeAlunos) = contarNotas turma
    
    contarNotas :: Turma -> (Float, Float)
    contarNotas Empty = (0,0)
    contarNotas (Node aluno e d) = 
        let (somaDasNotasE, totalE) = contarNotas e 
            (somaDasNotasD, totalD) = contarNotas d 
        in case aluno of 
            (_,_,_,Aprov nota) -> (fromIntegral nota + somaDasNotasE + somaDasNotasD, 1 + totalE + totalD)
            (_,_,_,_) -> (somaDasNotasE + somaDasNotasD, 1 + totalE + totalD)
            

    
