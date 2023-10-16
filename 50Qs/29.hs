uniao :: Eq a => [a] -> [a] -> [a]
uniao [] l = l
uniao l [] = l
uniao (x:xs) (h:t) = if pertence x (h:t) then uniao (h:t) xs
                     else x : uniao xs (h:t)


pertence :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence n (h:t) = (n == h) || pertence n t