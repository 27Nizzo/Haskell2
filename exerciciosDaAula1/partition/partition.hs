partition1 ::Ord a => a -> [a] -> ([a],[a])
partition1 x [] = ([],[])
partition1 x l = (mens x l, maios x l)

mens :: Ord a => a -> [a] -> [a]
mens x [] = []
mens x (h:t) = if h > x then mens x t
                            else h : mens x t 


maios :: Ord a => a -> [a] -> [a]
maios x [] = []
maios x (h:t) = if h <= x then maios x t
                                else h : maios x t