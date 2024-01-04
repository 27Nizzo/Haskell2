        type Nome = String 
        type Telefone = Integer 
        data Agenda = Vazia | Node (Nome, [Telefone]) Agenda Agenda 
        instance Show Agenda where
            show Vazia = ""
            show (Node (n,t) e d) = show e ++ "/" ++ show (n,t) ++ "/" ++ show d
-- Defina Agenda como uma instância da classe Show de forma a que a visualização da árvore resulte numa listagem da informação ordenada
-- por ordem alfabética e em que os vários telefone associados a um nome se apresentem separados por / 

