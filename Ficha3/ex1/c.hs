data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

horaValida :: Hora -> Bool 
horaValida (H 0 0) = True
horaValida (H a b) = if (a >= 0 && a <= 23) && (b >= 0 && b <= 59) 
                    then  True
                    else False 


horasMinutos :: Hora -> Int
horasMinutos (H 0 0) = 0
horasMinutos (H a b) = (a * 60) + b  

depois :: Hora -> Hora -> Bool
depois (H 0 0) (H 0 0) = False 
depois (H a b) (H as bs) = if (a < as) then True
                        else if (a == as) && (b < bs) then True
                        else False

-- Execicio: Calcular a hora de chegada

chegada :: Viagem -> Hora
chegada [] = (H 0 0)
chegada viagem = snd (last viagem)

-- Exercicio: Calcular a hora de partida

partida :: Viagem -> Hora
partida [] = (H 0 0)
partida viagem = fst (head viagem)