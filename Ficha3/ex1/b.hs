data Hora = (Int,Int)
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

horaValida :: Hora -> Bool 
horaValida (0,0) = True
horaValida (a,b) = if (a >= 0 && a <= 23) && (b >= 0 && b <= 59) 
                    then  True
                    else False 


horasMinutos :: Hora -> Int
horasMinutos (0,0) = 0
horasMinutos (a,b) = (a * 60) + b  

depois :: Hora -> Hora -> Bool
depois (0,0) (0,0) = False 
depois (a,b) (as,bs) = if (a < as) then True
                        else if (a == as) && (b < bs) then True
                        else False

-- Exercicio: Verificar se a viagem é valida 

viagemValida :: Viagem -> Bool
viagemValida [] = False  
viagemValida [v] = True  
viagemValida ((a,b) : (as,bs) : t) = if(horaValida a && horaValida b) && (depois as a) 
                                        then viagemValida((as,bs):t)
                                            else False  