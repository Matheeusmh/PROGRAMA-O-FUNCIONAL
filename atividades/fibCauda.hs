fibCauda :: Int -> Int
fibCauda x = fibAux x 0 1

fibAux :: Int -> Int -> Int -> Int
fibAux 0 a _ = a 
fibAux x a b = fibAux(x - 1) b (a + b)