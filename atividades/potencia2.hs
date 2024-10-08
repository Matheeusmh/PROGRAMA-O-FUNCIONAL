potenciaDois :: Integer -> Integer
potenciaDois x 
    |x == 0 = 1
    |x == 1 = 2
    |otherwise = 2 * potenciaDois (x - 1)