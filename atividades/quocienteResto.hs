divDef :: Int -> Int -> (Int, Int)
divDef a b 
    |a < b = (0, a)
    |a == 0 = (1, 0)
    |otherwise = (count, a - count * b)
        where
            count = 1 + div (a - b) b
