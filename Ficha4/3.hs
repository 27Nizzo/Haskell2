divModd' :: Integral a => a -> a -> (a, a)
divModd' _ 0 = error "Divisão por zero não é permitida."
divModd' a b
  | a < 0 && b > 0 || a > 0 && b < 0 = let (q, r) = divModd' (-a) (abs b) in (-q, r)
  | otherwise = go 0 (abs a) (abs b)
  where
    go q n m
      | n < m = (q, if a < 0 then -n else n)
      | otherwise = go (q + 1) (n - m) m
