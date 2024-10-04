prodMin :: Int -> Int -> Int
prodMin x y = prod [x..y]

prod :: [Int] -> Int
prod [] = error "Lista vazia!"
prod [a] = a
prod (a:as) = a * prod as