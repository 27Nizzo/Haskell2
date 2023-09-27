--vamos representar horas por um par de n´umeros inteiros: type Hora = (Int,Int)
--Assim o par (0,15) significa meia noite e um quarto e (13,45) duas menos um quarto.
--Defina fun¸c˜oes para: (a) testar se um par de inteiros representa uma hora do dia v´alida

hora:: (Int, Int) -> Bool
hora (x, y) = if (x >= 0 && x<= 24) && (y >= 0 && y <= 59)
                then True
                else False