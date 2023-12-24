    data Person = Person {  name :: String,
                             age :: Int }

greet1 :: Person -> [Char]
greet1 (Person name _) = "Hi " ++ name


    data Point = 
        D2 { x :: Int, y :: Int}
        |D3 { x :: Int, y :: Int, z :: Int }

-- Na consola:
-- x (D2 1 2) => 1
-- x (D3 1 2 3) => 1 
