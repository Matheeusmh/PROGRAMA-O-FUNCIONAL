fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat a = fatAux a 1
    where
        fatAux 1 x = x
        fatAux y x = fatAux (y - 1) (y * x)