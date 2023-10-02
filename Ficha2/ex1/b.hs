funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2) == 0 then h : (funB t)
                               else (funB t)

-- esta função verifica se algum inteiro da lista é par, 
-- se for par mete-o na output da função, se no caso for impar é ignorado.
-- Por exemplo: -> funB [1,2,3] = [2]
--              -> funB [2] = [2]
--              -> funB [1,3] = []
