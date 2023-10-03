somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h < 0 then h + somaNeg t
                else somaNeg t