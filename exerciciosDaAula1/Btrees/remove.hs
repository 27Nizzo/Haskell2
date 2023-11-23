data Btree a = Empty 
             | Node a (Btree a) (Btree a) deriving Show 

a, a1, a2 :: Btree Int
a = Node 30 a1 a2
a1 = Node 20 (Node 10 Empty Empty) Empty
a2 = Node 64 (Node 45 (Node 40 Empty Empty) (Node 50 Empty Empty)) (Node 78 Empty Empty)

removeRaiz :: Ord a => Btree a -> Btree a 
removeRaiz (Node r Empty d) = d
removeRaiz (Node r e Empty) = e
removeRaiz (Node r e d) = Node m e (remove m d)
  where m = maior e


maior :: Btree a -> a
maior (Node r e Empty) = r
maior (Node r Empty d) = maior d


remove :: Ord a => a -> Btree a -> Btree a 
remove x Empty = Empty 
remove x (Node r e d) | x == r = removeRaiz (Node r e d)
                      | x < r = Node r (remove x e) d
                      | x > r = Node r e (remove x d)