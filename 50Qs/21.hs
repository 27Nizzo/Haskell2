{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
isPrime :: Int -> Bool 
isPrime n =  if n <= 1 then False
        else if n <= 3 then True
        else if mod n 2 == 0 then False 

                    