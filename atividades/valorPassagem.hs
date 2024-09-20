passagem :: Int -> Double -> Double
passagem idade valor
    | idade > 59 = 0.6 * valor
    | idade > 1 && idade < 11 = valor / 2
    | idade < 2 = 0.1 * valor
    | otherwise = valor