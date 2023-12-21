    data ExpInt = const Int 
        | Mais ExpInt ExpInt 
        | Menos ExpInt ExpInt
        | Vezes ExpInt ExpInt
    

    calcula :: ExpInt -> Int 
    calcula (const x) = x 
    calcula (Mais c1 c2) = (calcula c1) + (calcula c2)
    calcula (Menos c1 c2) = (calcula c1) - (calcula c2)
    calcula (Vezes c1 c2) = (calcula c1) * (calcula c2)

