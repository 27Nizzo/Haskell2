data Btree a = Empty 
             | Node a (Btree a) (Btree a) deriving Show 

a, a1, a2 :: Btree Int
a = Node 30 a1 a2
a1 = Node 20 (Node 10 Empty Empty) Empty
a2 = Node 64 (Node 45 (Node 40 Empty Empty) (Node 50 Empty Empty)) (Node 78 Empty Empty)

removeRaiz :: Btree a -> Btree a 
removeRaiz (Node _ Empty d) = d
removeRaiz (Node _ e Empty) = e
removeRaiz (Node _ e d) = Node m e (remove m d)
  where m = maior e

maior :: Btree a -> a
maior (Node r _ Empty) = r
maior (Node _ _ d) = maior d

remove :: Ord a => a -> Btree a -> Btree a
remove _ Empty = Empty 
remove x (Node r e d) 
  | x == r = removeRaiz (Node r e d)
  | x < r = Node r (remove x e) d
  | otherwise = Node r e (remove x d)

removeP :: Ord a => a -> Btree a -> Btree a
removeP _ Empty = Empty 
removeP x (Node r e d) 
  | x == r = removeRaiz (Node r e d)
  | x < r = Node r (removeP x e) d 
  | x > r = Node r e (removeP x d)
