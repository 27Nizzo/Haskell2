data Movimento = Credito Float | Debito Float
    deriving Show

data Data = D Int Int Int
    deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

saldo :: Extracto -> Float
saldo (Ext saldoInicial movimentos) = calculaSaldo saldoInicial movimentos
  where
    calculaSaldo :: Float -> [(Data, String, Movimento)] -> Float
    calculaSaldo saldo [] = saldo
    calculaSaldo saldo ((_, _, movimento):restoMovimentos) = case movimento of
        Credito valor -> calculaSaldo (saldo + valor) restoMovimentos
        Debito valor -> calculaSaldo (saldo - valor) restoMovimentos
