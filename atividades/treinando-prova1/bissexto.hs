bissexto :: Int -> Bool
bissexto a = (mod a 4 == 0 && mod a 100 /= 0) || (mod a 400 == 0)
