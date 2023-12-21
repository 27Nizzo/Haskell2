    class Eq a where 
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool 
    
    data Exp = const Int 
            | Mais ExpInt ExpInt 
            | Menos ExpInt ExpInt
            | Vezes ExpInt ExpInt
 
    elem (Mais (const 3)(const 4))[const 2, const 3]

    instance Eq Exp where 
        (==) :: Exp -> Exp -> Bool 
        (const x) == (const y) = x == y
        (Mais c1 c2) == (Mais j1 j2) = (c1 == j1) && (c2 == j2)
        (Menos c1 c2) == (Menos j1 j2) = (c1 == j1) && (c2 == j2)
        (Vezes c1 c2) == (Vezes j1 j2) = (c1 == j1) && (c2 == j2)
        _ == _ = False 

    -- OU
    instance Eq Exp where 
            (==) :: Exp -> Exp -> Bool 
            e1 == e2 = calcula e1 == calcula e2

-- solução de subclasse 
    class Eq a => Ord a where 
        (<), (>), (<=), (>=) :: a -> a -> Bool 
        compare :: a -> a -> Ordening
        max,min :: a -> a -> a
        max a b = if a > b then a else b
        min a b = if a < b then a else b
    
    data Ordening = LT | Eq | GT

    class Show a where 
        Show :: a -> String 
        Show :: a -> String -> String 
        ShowPrec :: Int -> a -> String -> String

    instance Show Exp where 
        --Show (Mais (const 3) (const 2)) = "3 2 +"
        --Show :: Exp -> String 
        Show (const x) = Show x ++ ""
        Show (Mais e1 e2) = Show e1 ++ Show e2 ++ "+"
        Show (Menos e1 e2) = Show e1 ++ Show e2 ++ "-"
        Show (Vezes e1 e2) = Show e1 ++ Show e2 ++ "*"


        
