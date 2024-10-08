multIntervalo :: Int -> Int -> Int
multIntervalo m n 
    |n == 0 = 1
    |m == n = n
    |otherwise = m * multIntervalo (m + 1) n

fat :: Int -> Int
fat x = multIntervalo 1 x