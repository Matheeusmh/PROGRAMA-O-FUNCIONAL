par :: Int -> Bool
par x
    |x == 0 = True
    |x < 0 = par (-1 * x)
    |otherwise = not(par(x - 1))
