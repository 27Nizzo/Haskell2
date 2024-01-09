    data ExpInt = Const Int 
                | Simetrico 
                | Mais ExpInt ExpInt 
                | Menos ExpInt ExpInt 
                | Mult ExpInt ExpInt 

--A)
    calcula :: ExpInt -> Int 
    calcula (Const x) = x 
    calcula (Mais x1 x2) = (calcula x1) + (calcula x2)
    calcula (Menos x1 x2) = (calcula x1) - (calcula x2)
    calcula (Mult x1 x2) = (calcula x1) * (calcula x2)

--b)
    