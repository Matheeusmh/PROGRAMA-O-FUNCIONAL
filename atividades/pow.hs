pow :: (Int, Int) -> Int
pow (a, 0) = 1
pow (a, 1) = a
pow (a, b) = a * pow(a, (b - 1))