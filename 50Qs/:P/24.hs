isSubsquenceOf1 :: Eq a => [a] -> [a] -> Bool 
isSubsquenceOf1 [] _ = True 
isSubsquenceOf1 _ [] = False 
isSubsquenceOf1 (a:b) (as:bs) = if a == as then isSubsquenceOf1 b bs 
                                            else isSubsquenceOf1 (a:b) bs