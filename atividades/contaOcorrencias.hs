cont :: Int -> [Int] -> Int
cont x [] = 0
cont a (x:xs)
 |a == x = 1 + cont a xs
 |otherwise = cont a xs

unicaOc :: Int -> [Int] -> Bool
unicaOc x xs = cont x xs == 1