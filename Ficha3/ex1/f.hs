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

-- Exercicio: Calcular o tempo total da viagem(soma dos tempos de espera e de viagem efetiva)

tempoEf :: Viagem -> Int 
tempoEf [] = 0
tempoEf ((x,y):t) = (horasMinutos(y) - horasMinutos(x)) + tempoEf t

tempoEsp :: Viagem -> Int
tempoEsp [] = 0
tempoEsp ((H a b):(H as bs):t) = (horasMinutos(as) + bs) - (horasMinutos(a) + b) + tempoEsp t 

tempoTot :: Viagem -> Int
tempoTot [] = 0
tempoTot [viagem] = tempoEf viagem
tempoTot viagem = tempoEf viagem + tempoEsp viagem 