sumFats :: Int -> Int
sumFats x = sumFatsCount [0..x]

sumFatsCount :: [Int] -> Int
sumFatsCount (x:xs) = fat x + sumFatsCount xs

fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat x = x * fat (x-1)