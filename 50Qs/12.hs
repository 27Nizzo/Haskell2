--apresente uma função concat que concatena as listas de uma lista
-- concatt [[1],[2,2],[3,3,3],[4],[5,5],[4]]

concatt :: [[a]] -> [a]
concatt [] = []
concatt [[x]] = [x]
concatt (h:t) = h ++ concatt t