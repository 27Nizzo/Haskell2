funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t