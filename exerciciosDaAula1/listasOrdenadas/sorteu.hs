sorteu :: Ord a => [a] -> Bool
sorteu (x:y:t) = if (x <= y) then sorteu(y:t)      
                                else False