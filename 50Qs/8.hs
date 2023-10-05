-- função constroi uma lista de pares, recebe duas listas e cria uma nova lista juntando dois elementos de cada lista de acordo à sua posição 

zipp :: [a] -> [b] -> [(a,b)]
zipp [] _ = []
zipp _ [] = []
zipp (h:t) (h1:t1) = (h,h1) : zipp t t1

-- se der uma lista vazia devolve uma lista vazia e vice versa;
-- pegamos nas heads das listas e vamos "zipando" ao longo das duas tails 