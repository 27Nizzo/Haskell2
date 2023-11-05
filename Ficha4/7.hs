intToStr :: Integer -> String
intToStr n = intToStrAux n ""
  where
    intToStrAux 0 result = if null result then "0" else result
    intToStrAux num result
      | num < 0   = "-" ++ intToStrAux (-num) result
      | otherwise = intToStrAux (num `div` 10) (charDigit : result)
      where
        charDigit = toEnum (fromEnum '0' + fromIntegral (num `mod` 10))
