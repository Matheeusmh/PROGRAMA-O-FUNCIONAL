maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c = if a > b && a > c then a
                else if b > a && b > c then b
                else c
                
maximo3' :: Int -> Int -> Int -> Int
maximo3' a b c | a > b && a > c = a
               | b >a && b > c = b
               | otherwise = c