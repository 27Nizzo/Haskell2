--Def: high order function é uma função que utiliza a propria função como argumento

--Ex1:

app :: (a -> b) -> a -> b
app f x = f x 

add1 :: Int -> Int
add1 x = x + 1

-- app add1 x = add1 x = x + 1
-----------------------------
