
    type Mat = [[Int]]

{- 2)  O formato csv (comma separated values) serve para descrever tabelas de uma forma textual:
cada linha da tabela corresponde a uma linha do texto, enquanto que os elementos de cada linha
se encontram separados por v´ırgulas.
Por exemplo, a string "2,3,6,4\n12,3,12,4\n3,-4,5,7"
pode ser usada para descrever a matriz:

    [ 2 3 6 4  ]
    [ 12 3 12 4 ]
    [ 3 -4 5 7 ]

    a)  Considere o tipo type Mat = [[Int]] para representar matrizes e a seguinte defini¸c˜ao da
fun¸c˜ao stringToMat que converte strings desse formato em matrizes:
    stringToMat :: String -> Mat
    stringToMat s = map stringToVector (lines s)
-}

    stringToMat :: String -> Mat 
    stringToMat s = map stringToVector (lines s)

    stringToVector :: String -> [Int]
    stringToVector s = map read (words s)

--b) Defina a função transposta que recebe a tabela em formato textual e devolve a tabela transposta também em formato textual.

    transposta :: String -> String
    transposta s = unlines (map unwords (transpose (map words (lines s))))

    transpose :: [[a]] -> [[a]]
    transpose ([]:_) = []
    transpose x = (map head x) : transpose (map tail x)
    