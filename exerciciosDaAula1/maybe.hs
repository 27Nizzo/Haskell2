--Ex: 
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv a b = 
        if b == 0 then Nothing else Just $ div a b 
        