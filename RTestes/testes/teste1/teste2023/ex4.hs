--4) 
    data BTree a = Empty | Node a (BTree a) (BTree a) 

    inorder :: BTree a -> [a]
    inorder Empty = []
    inorder (Node r e d) = (inorder e) ++ (r:inorder d)

    --a) 
    {-Defina uma fun¸c˜ao numera :: BTree a -> BTree (a,Int) que coloca em cada nodo da
´arvore argumento o n´umero de ordem desse nodo numa travessia inorder. A fun¸c˜ao deve
percorrer a ´arvore uma ´unica vez.
Por exemplo, numera (Node ’a’ (Node ’b’ Empty Empty) (Node ’c’ Empty Empty))
deve dar como resultado (Node (’a’,2) (Node (’b’,1) Empty Empty) (Node (’c’,3)
Empty Empty))
Sugest˜ao: Comece por definir a fun¸c˜ao numeraAux :: Int -> BTree a -> (Int,BTree
(a,Int)) que recebe um inteiro (o primeiro n´umero a ser usado) e retorna a ´arvore numerada
bem como o n´umero de elementos dessa ´arvore.-}
    
    numera :: BTree a -> BTree (a,Int)
    numera t = snd (numeraAux 1 t)
    
    numeraAux :: Int -> BTree a -> (Int, BTree (a,Int))
    numeraAux n Empty = (n, Empty)
    numeraAux n (Node r e d) = 
        let (nexNumLeft, numLeft) = numeraAux n e 
            (nextNumRoot, numRoot) = (nextNumLeft + 1, Node (r, nextNumLeft) numLeft)
            (nextNumRight, numRight) = numeraAux nextNumRoot d
            in (nextNumRight, numRight)

--b) 
  {- A fun¸c˜ao inorder n˜ao ´e injectiva: h´a muitas ´arvores diferentes que d˜ao origem `a mesma
travessia: por exemplo, as ´arvores Node 1 Empty (Node 2 Empty Empty) e Node 2 (Node
1 Empty Empty) Empty tˆem como travessia a lista [1,2]
Defina a fun¸c˜ao unInorder :: [a] -> [BTree a] que, dada uma lista, calcula (a lista
de) todas as ´arvores cuja travessia inorder corresponde a essa lista.
-}

    unInorder :: [a] -> [BTree a]
    unInorder [] = [Empty] 
    unInorder xs = [Node r e d | i <- [0..length xs - 1],
                                r <- [xs !! i],
                                e <- unInorder (take i xs)
                                d <- unInorder (drop (i + 1) xs)]
