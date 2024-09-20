bonus :: Int -> Double
bonus i
 | i < 0 = 0  
 | i == 0 = 0
 | i > 0 && i < 11 = 100.00
 | i > 10 && i < 21 = 200.00
 | i > 20 && i < 31 = 300.00
 | i > 30 && i < 41 = 400.00
 | otherwise = 500.00