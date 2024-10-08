quadradoInt :: Int -> Int
quadradoInt x = quadradoAux x 1

quadradoAux :: Int -> Int -> Int
quadradoAux x count 
    |(count * count) > x = count - 1
    |otherwise = quadradoAux x (count + 1)