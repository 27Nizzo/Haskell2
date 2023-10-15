{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
isSuffixOf1 :: Eq a => [a] -> [a] -> Bool
isSuffixOf1 [] [] = True
isSuffixOf1 l [] = False
isSuffixOf1 (x:y) (xs:ys) = if x == xs then isSuffixOf1 y ys else isSuffixOf1(x:y) ys

-- é true quando [20,30] [10,20,30]
-- é false quando [10,30] [10,20,30]