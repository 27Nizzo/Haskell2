sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
--sumTriplos ((a1,b1,c1): t) = 
  --  let (a,b,c) = sumTriplos t
    --in (a1 + a, b1 + b, c1 + c)

sumTriplos((a,b,c):t) = (a + sumA, b + sumB, c + sumC)
  where (sumA,sumB,sumC) = sumTriplos t