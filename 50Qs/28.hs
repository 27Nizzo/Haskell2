(\\\) :: Eq a => [a] -> [a] -> [a]
(\\\) l [] = l
(\\\) [] _ = []
(\\\) (a:b) (h:t) = (\\\) (deletee1 h (a:b)) t

deletee1 :: Eq a => a -> [a] -> [a]
deletee1 _ [] = []
deletee1 n (h:t) = if n == h then t
                    else h : deletee1 n t