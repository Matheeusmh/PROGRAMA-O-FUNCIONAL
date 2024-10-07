mul :: Int -> Int -> Int
mul x 1 = x
mul x y = x + mul x (y - 1)