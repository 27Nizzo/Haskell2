isSuffixOf1 :: Eq a => [a]-> [a] -> Bool
isSuffixOf1 _ [] = error "O sufixo a ser testado excede a lista"
isSuffixOf1 lista1 lista2@(y:ys) = if length lista1 == length lista2
    then isPrefixOf1 lista1 lista2
    else isSuffixOf1 lista1 ys

    isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 [] l = True
isPrefixOf1 (a:as) (b:bs) = (a == b) && isPrefixOf1 as bs