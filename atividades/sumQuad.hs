sumQd :: Int -> Int
sumQd x = quad [0..x]

quad :: [Int] -> Int
quad [x] = x * x
quad (x:xs) = x * x + quad xs