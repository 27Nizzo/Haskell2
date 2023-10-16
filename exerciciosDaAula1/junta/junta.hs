 -- função que junta duas listas 

 junta :: [a] -> [a] -> [a]
 junta [] l2 = l2;
 junta (h1, t1) l2 = h1 : junta t1 l2 