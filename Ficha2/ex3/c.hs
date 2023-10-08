
nums :: String -> [Int]
nums "" = []
nums (s:ss) = if (s >= '0' && s <= '9') then (read [s] :: Int) : nums ss
                                        else nums ss