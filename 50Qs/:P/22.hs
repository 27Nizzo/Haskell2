isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 [] l = True
isPrefixOf1 (a:as) (b:bs) = (a == b) && isPrefixOf1 as bs