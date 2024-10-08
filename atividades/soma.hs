soma :: Int -> Int -> Int
soma x y 
    |y == 0 = x
    |y > 0 = soma (succ x) (pred y)
    |otherwise = soma (pred x) (succ y)