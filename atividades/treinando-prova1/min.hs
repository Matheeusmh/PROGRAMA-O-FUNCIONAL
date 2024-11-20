min2 :: Int -> Int -> Int
min2 a b 
 |a < b = a
 |otherwise = b

minTres :: Int -> Int -> Int -> Int
minTres x y z = min2 x (min2 y z)
