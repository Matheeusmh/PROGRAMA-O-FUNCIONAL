maximo2 :: Int -> Int -> Int
maximo2 a b = if a > b then a
             else b

maximo2' :: Int -> Int -> Int
maximo2' a b |a > b = a
             |otherwise = b