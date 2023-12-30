--1) 
    data ExpInt = Const Int 
                | Simetrico ExpInt
                | Mais ExpInt ExpInt 
                | Menos ExpInt ExpInt 
                | Mult ExpInt ExpInt 

--a)
    calcula :: ExpInt -> Int 
    calcula (Const  c) = c 
    calcula (Mais c1 c2) = (calcula c1) + (calcula c2)
    calcula (Menos c1 c2) = (calcula c1) - (calcula c2)
    calcula (Mult c1 c2) = (calcula c1) * (calcula c2)

    {- No terminal:
 calcula (Mais (Const 1) (Const 2)) -}

 --b) 
    instance Show ExpInt where 
        -- Show (Mais (Const 3) (Const 2)) = "3 + 2"
        -- Show :: Exp -> String 
        show (Const c) = show c
        show (Simetrico c) = "(" ++ " - " ++ show c ++ ")" 
        show (Mais c1 c2) = "(" ++ show c1 ++ " + " ++ show c2 ++ ")"
        show (Menos c1 c2) = "(" ++ show c1 ++ " - " ++ show c2 ++ ")"
        show (Mult c1 c2) = "(" ++ show c1 ++ " * " ++ show c2 ++ ")"

    infixa :: ExpInt -> String 
    infixa (Const c) = show c 
    infixa (Mais c1 c2) = "(" ++ infixa c1 ++ " + " ++ infixa c2 ++ ")" ++ " = " ++ show (calcula (Mais c1 c2))
    infixa (Menos c1 c2) = "(" ++ infixa c1 ++ " - " ++ infixa c2 ++ ")" ++ " = " ++ show (calcula (Menos c1 c2))
    infixa (Mult c1 c2) = "(" ++ infixa c1 ++ " * " ++ infixa c2 ++ ")" ++ " = " ++ show (calcula (Mult c1 c2))


--c) 
    posFixa :: ExpInt -> String 
    posFixa (Const c) = show c
    posFixa (Mais c1 c2) = "(" ++ show c1 ++ " " ++ show c2 ++ " +" ++ ")"
    posFixa (Menos c1 c2) = "(" ++ show c1 ++ " " ++ show c2 ++ " -" ++ ")"
    posFixa (Mult c1 c2) = "(" ++ show c1 ++ " " ++ show c2 ++ " *" ++ ")"