-- defina a função cola que recebe uma lista de triplos e cocatena as strings que estão na primeira componente dos triplos

cola :: [(String,b,c)] -> String
cola [] = ""
cola [("",b,c)] = ""
cola ((a,b,c):t) = a ++ cola t
