
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit [] = 0
maxSumInit (x:xs) = maxSumInitAux x xs x
    where 
    maxSumInitAux maxSum [] _ = maxSum
    maxSumInitAux maxSum (y:ys) currentSum = 
        let newSum = max y (currentSum + y)
        in maxSumInitAux (max maxSum newSum) ys newSum