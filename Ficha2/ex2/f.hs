tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) = if length((h:t)) <= 3 then (h:t)
                                else tresUlt t
