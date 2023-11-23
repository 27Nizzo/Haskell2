-- a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]

-- R: Esta lista contem inteiros de 1 a 20 e vai me devolver os numeros que são divisiveis por 2 e por 3 em simultâneo, o resultado final vai ser 6 12 18 

-- b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]

-- R: Esta lista contem inteiros de 1 a 20 e vai primeiramente verificar quem é que é divisivel por 2 e dps vai verificar se esses mesmos numeros são divisiveis por 3

-- c) [(x,y) | x <- [0..20], y <-[0..20], x + y == 30]

-- R: Vai receber um par x e y onde os dois são listas de inteiros de 0 a 20, e o programa vai verificar quais são os pares somados um com o outro onde o seu resultado é igual a 30

-- d) [sum[y | y <-[1..x], odd y] | x <- [1..10]]

-- R: Esta função vai fazer a soma dos elementos impares de y, ou seja, 1,3,5,7,9 com os elementos de x de 1 a 10, ou seja, será 1+1, 3+2, 5+3, 7+4 e 9+5.
