fatDuplo :: Int -> Int
fatDuplo 0 = 1
fatDuplo 1 = 1
fatDuplo x = x * fatDuplo(x - 2)