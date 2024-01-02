    type Mat = [[Int]]

--Apresente uma defini¸c˜ao recursiva da fun¸c˜ao unlines :: [String] -> String que junta todas as strings da lista numa s´o, separando-as pelo caracter ’\n’.

--Por exemplo, unlines ["Prog", "Func"] == "Prog\nFunc".

--1) 
    unlines' :: [String] -> String
    unlines' [] = ""
    unlines' [x] = x 
    unlines' (h:t) = h ++ "\n" ++ unlines' t






                            
