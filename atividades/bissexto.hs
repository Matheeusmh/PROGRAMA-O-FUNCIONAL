bissexto :: Int -> Bool
bissexto x = if ((x `mod` 4 == 0 && x `mod` 100 /= 0) || (x `mod` 4 == 0 &&
 x `mod` 100 == 0 && x `mod` 400 == 0)) then True
 else False
