concatena :: [Int] -> [Int] -> [Int]
concatena [] l = l
concatena (x:xs) l = x: concatena xs l