--1) Create a function rev that reverses a list 
-- para foldr 
rev :: [a] -> [a]
rev lista = foldr (\x acc -> acc ++ [x]) [] lista

-- para foldl
rev2 :: [a] -> [a]
rev2 = foldl (\acc x -> x : acc) []

rev3 :: [a] -> [a]
rev3 = foldl (flip(:)) []

--2) Create a function prefixes that returns all the prefixes of a given list

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : (map((:) x) acc)) []

--prefixes [1,2,3] => [[1],[1,2],[1,2,3]]

--3) Create the function of Lagrange


lagrange :: [(Float, Float)] -> Float -> Float 
lagrange xs x = foldl (\acc (xj,y) -> acc + (y * l xj)) 0 xs 
    where 
        l xj = foldl (\acc (xk, _) -> 
            if xj == xk then acc  
                        else acc * ((x - xk)/(xj - xk))) 1 xs 

--4) Create a function foldTrie that folds the elements of a trie in a
--   preorder transversal

data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving Show

foldBTree :: (b -> a -> b) -> b -> BTree a -> b
foldBTree f acc Empty = acc
foldBTree f acc (Node r e d) =
  let acc' = f acc r
  in foldBTree f (foldBTree f acc' e) d

-- Exemplo de uso:
somaBTree :: BTree Int -> Int
somaBTree tree = foldBTree (+) 0 tree

-- Exemplo de uma arvore binaria: 

exemploBTree :: BTree Int 
exemploBTree = 
        Node 1 
            (Node 2
                (Node 3 Empty Empty)
                    (Node 4 Empty Empty))
                                    (Node 5
                                         (Node 6 Empty Empty)
                                                            Empty)

somaExemplo :: Int
somaExemplo = somaBTree exemploBTree

