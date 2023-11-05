fib :: Int -> Int 
fib n = auxFib n 1 1

auxFib :: Int -> Int -> Int -> Int 
auxFib 1 acc1 _ = acc1 
auxFib n acc1 acc2 = auxFib (n - 1) (acc2) (acc2 + acc1)