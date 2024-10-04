maximo2 :: Int -> Int -> Int
maximo2 a b = if a > b then a
             else b

maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c = if a > b && a > c then a
                else if b > a && b > c then b
                else c

maximo3' :: Int -> Int -> Int -> Int
maximo3' a b c | a > b && a > c = a
               | b >a && b > c = b
               | otherwise = c

maximo3'' :: Int -> Int -> Int -> Int
maximo3'' a b c = maximo2 c (maximo2 a b)