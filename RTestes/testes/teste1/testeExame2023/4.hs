--4) 
    data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop

    p1 :: Prop 
    p1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))
    
    --a) 
    eval :: [(String,Bool)] -> Prop -> Bool 
    eval l (Not p) = not (eval l p) 
    eval l (And p1 p2) = (eval l p1) && (eval l p2) 
    eval l (Or p1 p2) = (eval l p1) || (eval l p2)


    --b) 
    nnf :: Prop -> Prop
    nnf (Var x) = Var x 
    nnf (Not (Not p)) = p 
    nnf (Not (And p1 p2)) = Or (nnf (Not p1)) (nnf (Not p2))
    nnf (Not (Or p1 p2)) = And (nnf (Not p1)) (nnf (Not p2))
    nnf (And p1 p2) = And (nnf p1) (nnf p2)
    nnf (Or p1 p2) = Or (nnf p1) (nnf p2)
    nnf p = p 


