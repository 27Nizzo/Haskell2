takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f [] = []
takeWhile1 f (h:t) = if f h then h : takeWhile1 f t 
                    else takeWhile1 f t

