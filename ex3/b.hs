-- (b) testar se uma hora ´e ou n˜ao depois de outra (compara¸c˜ao)(
-- Raciocinio: se a primeira hora for menor que a segunda entao vem dps da outra

hora :: (Int, Int) -> (Int, Int) -> Bool
hora (x1, y1) (x2, y2) = if (x1 < x2) 
                            then True
                            else if (x1 == x2) && (y1 < y2)
                                then True
                                else False
                                