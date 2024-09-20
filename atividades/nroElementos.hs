nroElemento :: [a] -> Int
nroElemento [] = 0
nroElemento (_:xs) = 1 + nroElemento xs