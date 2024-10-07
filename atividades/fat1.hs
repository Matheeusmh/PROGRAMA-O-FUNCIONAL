fat :: Int a => Int -> Int
fat 0 = 1
fat 1 = 1
fat a = a * fat (a - 1)