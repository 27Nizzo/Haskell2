-- exemplos de DataTypes:
--1) 
data Color =    
    Red | Orange | Yellow | Green | Blue | Magenta

--2)
data PeaNum = 
    Succ PeaNum | Zero 
four :: PeaNum 
four = Succ $ Succ $ Succ $ Zero 

incr :: PeaNum -> PeaNum
incr = Succ 

decr :: PeaNum -> PeaNum
decr (Succ n) = n 

add :: PeaNum -> PeaNum -> PeaNum 
add Zero n = n 
add (Succ m) n = Succ $ add m n 

sum :: [PeaNum] -> PeaNum 
sum []          = Zero 
sum (x:xs)      = add x $ sum xs 


--3) 
data Calculation = 
    Add Int Int | Sub Int Int | Mult Int Int | Div Int Int

-- Vamos utilizar o 3) 

calc :: Calculation -> Int 
calc (Add x y) = x + y 
calc (Sub x y) = x - y 
calc (Mult x y) = x * y 
calc (Div x y) = x / y 

-- Árvores: 

data Tree a = Leaf | Node (Tree a) a (Tree a) 

tree :: Tree Int 
tree = 
    Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)