
    intersect2 :: Eq a => [a] -> [a] -> [a]
    intersect2 [] _ = []
    intersect2 (h:t) ts 
            | elem h ts = h : intersect2 t ts 
            | otherwise = intersect2 t ts 

    -- em "elem h ts" a função elem esta a comparar se existe o elemento h na lista ts, se sim h vai para uma nova lista e dps vai fazer o mesmo processo que fez
    --em h em t 
    