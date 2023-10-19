deletee :: Eq a => a -> [a] -> [a]
deletee _ [] = []
deletee n (h:t:ts)
  | n == h = t:ts
  | n == t = h : ts