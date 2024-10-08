euclides :: Int -> Int -> Int
euclides a b
    |b == 0 = a 
    |b > 0 = euclides b (mod a b)
    |otherwise = euclides a (-b)

euclidesPlus :: Int -> Int -> Int -> Int
euclidesPlus a b c = euclides a (euclides b c)